import settings
import socket
import random
from urllib2 import urlopen
from common.funcs import QueryHandler
import time
from server_component import ServerComponent
class GroupCommentary(ServerComponent):
	"""
	This class will post commentary in the collection nodes, which will 
	post commentary in the respective groups
	"""
	def __init__(self, commentary, match_id):
		self.jid = "group_commentary@commentary.mm.io"
		self.password = "awestrtyuioi"
		self.server = "127.0.0.1"
		self.pubsub_node_id = settings.PUBSUB_COMMENTARY__NODE_NAME_SUFFIX + "match_id"
		self.port = 8888
		self.commentary = commentary
		self.match_id = match_id
		super(GroupCommentary, self).__init__(self.jid, self.password, "localhost", self.port)
		self.xmpp.register_plugin('xep_0060')


	def handleXMPPConnected(self, message):
		self.create()

	def post(self):
		pass

	def create(self):
		creation_stanza = "<iq type='set'\
		    from='{}'\
		    to='pubsub.mm.io'\
		    id='create1'>\
		  	<pubsub xmlns='http://jabber.org/protocol/pubsub'>\
		    <create node='{}'/>\
		    <configure>\
		      <x xmlns='jabber:x:data' type='submit'>\
		        <field var='FORM_TYPE' type='hidden'>\
		          <value>http://jabber.org/protocol/pubsub#node_config</value>\
		        </field>\
		        <field var='pubsub#persist_items'><value>0</value></field>\
		        <field var='pubsub#access_model'><value>open</value></field>\
		        <field var='pubsub#publish_model'><value>owner</value></field>\
		        <field var='pubsub#send_last_published_item'><value>never</value></field>\
		      </x>\
		    </configure>\
		  	</pubsub>\
			</iq>".format(self.jid, self.pubsub_node_id)
		self.xmpp.send_raw(creation_stanza)
		self.xmpp.disconnect()


def main():
	g = GroupCommentary("Hi", "1234")
	g.send()
	g.create()

if __name__ == '__main__':
	main()