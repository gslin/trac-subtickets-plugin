# -*- coding: utf-8 -*-
#
# Copyright (c) 2010, Takashi Ito
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of the authors nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

from trac.config import Option, IntOption, ChoiceOption, ListOption
from trac.core import Component, implements
from trac.web.api import IRequestFilter
from trac.web.chrome import ITemplateProvider, add_script, add_script_data, add_stylesheet
from trac.util.html import html as tag
from trac.ticket.api import ITicketManipulator
from trac.ticket.model import Type as TicketType
from .api import NUMBERS_RE, _


class SubTicketsModule(Component):

    implements(IRequestFilter, ITicketManipulator, ITemplateProvider)

    # Simple Options

    opt_skip_validation = ListOption(
        'subtickets', 'skip_closure_validation', default=[], doc=_("""
         Normally, reopening a child with a `closed` parent will be
         refused and closing a parent with non-`closed` children will also
         be refused. Adding either of `reopen` or `resolve` to this option will
         make Subtickets skip this validation for the respective action.
         Separate by comma if both actions are listed.

         Caveat: This functionality will be made workflow-independent in a
         future release of !SubTicketsPlugin.
         """))

    opt_recursion_depth = IntOption(
        'subtickets', 'recursion_depth', default=-1, doc=_("""
         Limit the number of recursive levels when listing subtickets.
         Default is infinity, represented by`-1`. The value zero (0)
         limits the listing to immediate children.
         """))

    opt_add_style = ChoiceOption('subtickets', 'add_style', ['button', 'link'],
                                 doc=_("""
         Choose whether to make `Add` look like a button (default) or a link
         """)
                                 )

    opt_owner_url = Option('subtickets', 'owner_url',
                           doc=_("""
                           Currently undocumented.
                           """)
                           )

    # Per-ticket type options -- all initialised in __init__()

    opt_inherit_fields = dict()
    opt_columns = dict()

    def _add_per_ticket_type_option(self, ticket_type):

        self.opt_inherit_fields[ticket_type] = ListOption(
            'subtickets', 'type.%s.child_inherits' % ticket_type, default='',
            doc=_("""Comma-separated list of ticket fields whose values are
            to be copied from a parent ticket into a newly created
            child ticket.
            """))

        self.opt_columns[ticket_type] = ListOption(
            'subtickets', 'type.%s.table_columns' % ticket_type,
            default='status,owner', doc=_("""
             Comma-separated list of ticket fields whose values are to be
             shown for each child ticket in the subtickets list
             """))

    def __init__(self):
        # The following initialisations must happen inside init()
        # in order to be able to access self.env
        for tt in TicketType.select(self.env):
            self._add_per_ticket_type_option(tt.name)

    # ITemplateProvider methods

    def get_htdocs_dirs(self):
        from pkg_resources import resource_filename
        return [('subtickets', resource_filename(__name__, 'htdocs'))]

    def get_templates_dirs(self):
        return []

    # IRequestFilter methods

    def pre_process_request(self, req, handler):
        return handler

    def post_process_request(self, req, template, data, content_type):
        path = req.path_info

        if path.startswith('/ticket/') or path.startswith('/newticket'):
            # get parent ticket's data
            if data and 'ticket' in data:
                ticket = data['ticket']
                parents = ticket['parents'] or ''
                ids = set(NUMBERS_RE.findall(parents))

                if len(parents) > 0:
                    self._append_parent_links(req, data, ids)

                children = self.get_children(ticket.id)
                if children:
                    data['subtickets'] = children

                # Generate HTML string and fill into script data.

                button = None
                link = None

                div = tag.div(class_='description')
                if 'TICKET_CREATE' in req.perm(ticket.resource) \
                        and ticket['status'] != 'closed':
                    opt_inherit = self.env.config.getlist(
                        'subtickets', 'type.%(type)s.child_inherits' % ticket)
                    if self.opt_add_style == 'link':
                        inh = {f: ticket[f] for f in opt_inherit}
                        link = tag.a(_('add'),
                                     href=req.href.newticket(parents=ticket.id,
                                                             **inh))
                        link = tag.span('(', link, ')', class_='addsubticket')
                    else:
                        inh = [tag.input(type='hidden',
                                         name=f,
                                         value=ticket[f]) for f in opt_inherit]

                        button = tag.form(
                            tag.div(
                                tag.input(type="submit",
                                          value=_("Create"),
                                          title=_("Create a child ticket")),
                                inh,
                                tag.input(type="hidden",
                                          name="parents",
                                          value=str(ticket.id)),
                                class_="inlinebuttons"),
                            method="get", action=req.href.newticket())
                div.append(button)
                div.append(tag.h3(_('Subtickets '), link))

            if 'subtickets' in data:
                # table
                tbody = tag.tbody()
                div.append(tag.table(tbody, class_='subtickets'))
                # tickets
                all_ids = self._collect_ids(data['subtickets'])
                tickets_data = self._batch_load_ticket_data(all_ids)
                self._create_subtickets_table(req, data['subtickets'],
                                              tbody, tickets_data)

            add_stylesheet(req, 'subtickets/css/subtickets.css')
            add_script(req, 'subtickets/js/subtickets.js')
            add_script_data(req, subtickets_div=str(div))

        elif path.startswith('/admin/ticket/type') \
                and data \
                and set(['add', 'name']).issubset(data.keys()) \
                and data['add'] == 'Add':
            self._add_per_ticket_type_option(data['name'])

        return template, data, content_type

    def _batch_load_ticket_data(self, ticket_ids):
        """Batch load ticket data for multiple IDs."""
        if not ticket_ids:
            return {}
        id_list = list(ticket_ids)
        rows = self.env.db_query("""
            SELECT id, type, status, summary, owner, milestone
            FROM ticket WHERE id IN %s
            """, (id_list,))
        result = {}
        for row in rows:
            result[row[0]] = {
                'type': row[1], 'status': row[2], 'summary': row[3],
                'owner': row[4], 'milestone': row[5],
            }
        custom_rows = self.env.db_query("""
            SELECT ticket, name, value FROM ticket_custom
            WHERE ticket IN %s
            """, (id_list,))
        for ticket_id, name, value in custom_rows:
            if ticket_id in result:
                result[ticket_id][name] = value
        return result

    def _append_parent_links(self, req, data, ids):
        int_ids = [int(i) for i in ids]
        tickets_data = self._batch_load_ticket_data(int_ids)
        links = []
        for id in sorted(int_ids):
            if id not in tickets_data:
                continue
            td = tickets_data[id]
            elem = tag.a('#%s' % id,
                         href=req.href.ticket(id),
                         class_='%s ticket' % td['status'],
                         title=td['summary'])
            if len(links) > 0:
                links.append(', ')
            links.append(elem)
        for field in data.get('fields', ''):
            if field.get('name') == 'parents':
                field['rendered'] = tag.span(*links)

    # ITicketManipulator methods

    def prepare_ticket(self, req, ticket, fields, actions):
        pass

    def get_children(self, parent_id, depth=0):
        """Get children tree using batch queries per level."""
        result = {}
        refs = {parent_id: result}
        current_parents = [parent_id]
        visited = {parent_id}

        d = 0
        while current_parents and (self.opt_recursion_depth == -1 or d <= self.opt_recursion_depth):
            rows = self.env.db_query("""
                SELECT parent, child FROM subtickets WHERE parent IN %s
                """, (current_parents,))

            next_parents = []
            for parent, child in rows:
                if child in visited:
                    continue
                visited.add(child)
                child_dict = {}
                refs[parent][child] = child_dict
                refs[child] = child_dict
                next_parents.append(child)

            current_parents = next_parents
            d += 1

        return result

    def validate_ticket(self, req, ticket):
        action = req.args.get('action')

        if action in self.opt_skip_validation:
            return

        if action == 'resolve':
            rows = self.env.db_query("""
                SELECT s.child FROM subtickets s
                JOIN ticket t ON s.child = t.id
                WHERE s.parent=%s AND t.status != 'closed'
                """, (ticket.id,))
            for child, in rows:
                yield None, _("""Cannot close/resolve because child
                     ticket #%(child)s is still open""",
                              child=child)

        elif action == 'reopen':
            ids = list(set(NUMBERS_RE.findall(ticket['parents'] or '')))
            if ids:
                int_ids = [int(i) for i in ids]
                rows = self.env.db_query("""
                    SELECT id FROM ticket
                    WHERE id IN %s AND status = 'closed'
                    """, (int_ids,))
                for id, in rows:
                    msg = _("Cannot reopen because parent ticket #%(id)s "
                            "is closed", id=id)
                    yield None, msg

    def _collect_ids(self, children):
        """Collect all ticket IDs from a children tree."""
        ids = set()
        for id, subtree in children.items():
            ids.add(id)
            if subtree:
                ids.update(self._collect_ids(subtree))
        return ids

    def _create_subtickets_table(self, req, children, tbody,
                                 tickets_data, depth=0):
        """Recursively create list table of subtickets."""
        if not children:
            return
        for id in sorted(children, key=lambda x: int(x)):
            td = tickets_data.get(id, {})

            # the row
            r = []
            # Always show ID and summary
            attrs = {'href': req.href.ticket(id)}
            if td.get('status') == 'closed':
                attrs['class_'] = 'closed'
            link = tag.a('#%s' % id, **attrs)
            summary = tag.td(link, ': %s' % td.get('summary', ''),
                             style='padding-left: %dpx;' % (depth * 15))
            r.append(summary)

            # Add other columns as configured.
            ticket_type = td.get('type', '')
            for column in \
                    self.env.config.getlist('subtickets',
                                            'type.%s.table_columns'
                                            % ticket_type):
                if column == 'owner':
                    owner = td.get('owner', '')
                    if self.opt_owner_url:
                        href = req.href(self.opt_owner_url % owner)
                    else:
                        href = req.href.query(status='!closed', owner=owner)
                    e = tag.td(tag.a(owner, href=href))
                elif column == 'milestone':
                    milestone = td.get('milestone', '')
                    href = req.href.query(status='!closed',
                                          milestone=milestone)
                    e = tag.td(tag.a(milestone, href=href))
                else:
                    e = tag.td(td.get(column, ''))
                r.append(e)
            tbody.append(tag.tr(*r))

            self._create_subtickets_table(req, children[id], tbody,
                                          tickets_data, depth + 1)
