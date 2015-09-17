# import sleekxmpp
# from sleekxmpp.xmlstream.stanzabase import ElementBase, ET, JID
# from sleekxmpp.stanza.iq import Iq

# class Pubsub(ElementBase):
#     namespace = 'http://jabber.org/protocol/pubsub'
#     name = 'param'
#     plugin_attrib = 'param'
#     interfaces = set(('name', 'value'))
#     sub_interfaces = interfaces
#     subitem = (Publish,)

# class Task(ElementBase):
#     namespace = 'example:task'
#     name = 'task'
#     plugin_attrib = 'task'
#     interfaces = set(('id', 'command', 'cleanup', 'params'))
#     sub_interfaces = set(('command', 'cleanup'))
#     subitem = (Param,)


#   <pubsub xmlns='http://jabber.org/protocol/pubsub'>
#     <publish node='princely_musings'>
#       <item>
#         <entry >
#           <title>Soliloquy</title>
#           <summary>
# To be, or not to be: that is the question:
# Whether 'tis nobler in the mind to suffer
# The slings and arrows of outrageous fortune,
# Or to take arms against a sea of troubles,
# And by opposing end them?
#           </summary>
#           <link rel='alternate' type='text/html'
#                 href='http://denmark.lit/2003/12/13/atom03'/>
#           <id>tag:denmark.lit,2003:entry-32397</id>
#           <published>2003-12-13T18:30:02Z</published>
#           <updated>2003-12-13T18:30:02Z</updated>
#         </entry>
#       </item>
#     </publish>
#   </pubsub>
# </iq>



#     <iq type="set">
#   <task id="123" xmlns="example:task">
#     <command>python script.py</command>
#     <cleanup>rm temp.txt</cleanup>
#     <param>
#       <name>foo</name>
#       <value>fizz</value>
#     </param>
#     <param>
#       <name>bar</name>
#       <value>buzz</value>
#     </param>
#   </task>
# </iq>