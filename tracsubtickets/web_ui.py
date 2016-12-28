#!/usr/bin/python
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
from trac.core import *
from trac.web.api import IRequestFilter, ITemplateStreamFilter
from trac.web.chrome import ITemplateProvider, add_stylesheet
from trac.ticket.api import ITicketManipulator
from trac.ticket.model import Ticket
from trac.ticket.model import Type as TicketType
from trac.resource import ResourceNotFound
from genshi.builder import tag
from genshi.filters import Transformer

from api import NUMBERS_RE, _

class SubTicketsModule(Component):

    implements(ITemplateProvider,
               IRequestFilter,
               ITicketManipulator,
               ITemplateStreamFilter)

    ### Simple Options

    opt_recursion_depth = IntOption('subtickets', 'recursion_depth', default=-1, 
                                    doc = _("""
                                    Limit the number of recursive levels when listing
                                    subtickets. Default is infinity, represented by
                                    `-1`. The value zero (0) limits the listing to
                                    immediate children.
                                    """)
                                    )
    opt_add_style = ChoiceOption('subtickets', 'add_style', ['button', 'link'],
                                 doc = _("""
                                 Choose whether to make `Add` look like a button (default)
                                 or a link
                                 """)
                                 )
    opt_owner_url = Option('subtickets', 'owner_url',
                           doc = _("""
                           Currently undocumented.
                           """)
                           )

    ### Per-ticket options -- all initialised in __init__()

    opt_inherit_fields = dict() 
    opt_columns = dict()

    ###

    def __init__(self):
        # The following initialisations must happen inside init() in order to access self.env
        for tt in TicketType.select(self.env):
            self.opt_inherit_fields[tt.name] = ListOption('subtickets','%s.child_inherits' % tt.name,
                                        default='', doc = _("""
                                        Comma-separated list of ticket fields whose values are
                                        to be copied from a parent ticket into a newly created
                                        child ticket
                                        """)
                                        )
            self.opt_columns[tt.name] = ListOption('subtickets', '%s.table_columns' % tt.name, 
                                 default='status,owner',
                                 doc = _("""
                                 Comma-separated list of ticket fields whose values are to be
                                 shown for each child ticket in the subtickets list
                                 """)
                                 )



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

        return template, data, content_type

    def _append_parent_links(self, req, data, ids):
        links = []
        for id in sorted(ids, key=lambda x: int(x)):
            try:
                ticket = Ticket(self.env, id)
                elem = tag.a('#%s' % id,
                             href=req.href.ticket(id),
                             class_='%s ticket' % ticket['status'],
                             title=ticket['summary'])
                if len(links) > 0:
                    links.append(', ')
                links.append(elem)
            except ResourceNotFound, e:
                pass
        for field in data.get('fields', ''):
            if field.get('name') == 'parents':
                field['rendered'] = tag.span(*links)

    # ITicketManipulator methods
    def prepare_ticket(self, req, ticket, fields, actions):
        pass

    def get_children(self, parent_id):
        with self.env.db_query as db:
            children = {}
            cursor = db.cursor()
            cursor.execute("""
                SELECT parent, child FROM subtickets WHERE parent=%s
                """, (parent_id, ))

            for parent, child in cursor:
                children[child] = None

            for id in children:
                children[id] = self.get_children(id)

            return children

    def validate_ticket(self, req, ticket):
        action = req.args.get('action')
        if action == 'resolve':
            with self.env.db_query as db:
                cursor = db.cursor()
                cursor.execute("""
                    SELECT parent, child FROM subtickets WHERE parent=%s
                    """, (ticket.id, ))

                for parent, child in cursor:
                    if Ticket(self.env, child)['status'] != 'closed':
                        yield None, _("Cannot close/resolve because child ticket #%(child)s is still open", child=child)

        elif action == 'reopen':
            ids = set(NUMBERS_RE.findall(ticket['parents'] or ''))
            for id in ids:
                if Ticket(self.env, id)['status'] == 'closed':
                    yield None, _("Cannot reopen because parent ticket #%(id)s is closed", id=id)

    # ITemplateStreamFilter method
    def filter_stream(self, req, method, filename, stream, data):
        if req.path_info.startswith('/ticket/'):
            div = None
            if 'ticket' in data:
                # get parents data
                ticket = data['ticket']
                # title
                div = tag.div(class_='description')
                if 'TICKET_CREATE' in req.perm(ticket.resource) and ticket['status'] != 'closed':
                    opt_inherit = self.env.config.getlist('subtickets', 
                                                          '%(type)s.child_inherits' % ticket)
                    if self.opt_add_style == 'link':
                        inh  = {f: ticket[f] for f in opt_inherit}
                        link = tag.a('add', 
                                     href=req.href.newticket(parents=ticket.id, *inh))
                        link = tag.span('(', link, ')', class_='addsubticket')
                        button = None
                    else:
                        inh = [tag.input(type  = 'hidden', 
                                         name  = f, 
                                         value = ticket[f]) for f in opt_inherit]

                        link = None
                        button = tag.form(tag.div(tag.input(type="submit", 
                                                            value="Create", 
                                                            title="Create a child ticket"),
                                                  inh,
                                                  tag.input(type="hidden", 
                                                            name="parents", 
                                                            value=str(ticket.id)),
                                                  class_="inlinebuttons"),
                                          method="get", action=req.href.newticket())
                else:
                    link = None
                    button = None
                div.append(button)
                div.append(tag.h3(_('Subtickets '), link))

            if 'subtickets' in data:
                # table
                tbody = tag.tbody()
                div.append(tag.table(tbody, class_='subtickets'))
                # tickets
                def _func(children, depth=0):
                    print '_func', depth, children
                    for id in sorted(children, key=lambda x: int(x)):
                        ticket = Ticket(self.env, id)

                        # the row
                        r = []
                        # Always show ID and summary
                        attrs = {'href': req.href.ticket(id)}
                        if ticket['status'] == 'closed':
                            attrs['class_'] = 'closed'
                        link = tag.a('#%s' % id, **attrs)
                        summary = tag.td(link, ': %s' % ticket['summary'],
                            style='padding-left: %dpx;' % (depth * 15))
                        r.append(summary)

                        # Add other columns as configured.
                        for column in self.env.config.getlist('subtickets', 
                                                              '%(type)s.table_columns' % ticket):
                            if column == 'owner':
                                if self.opt_owner_url:
                                    href = req.href(self.opt_owner_url % ticket['owner'])
                                else:
                                    href = req.href.query(status='!closed',
                                                          owner=ticket['owner'])
                                e = tag.td(tag.a(ticket['owner'], href=href))
                            elif column == 'milestone':
                                href = req.href.query(status='!closed',
                                                      milestone=ticket['milestone'])
                                e = tag.td(tag.a(ticket['milestone'], href=href))
                            else:
                                e = tag.td(ticket[column])

                            r.append(e)

                        tbody.append(tag.tr(*r))

                        if self.opt_recursion_depth > depth or self.opt_recursion_depth == -1 :
                            _func(children[id], depth + 1)

                _func(data['subtickets'])

            if div:
                add_stylesheet(req, 'subtickets/css/subtickets.css')
                stream |= Transformer('.//div[@id="ticket"]').append(div)

        return stream

