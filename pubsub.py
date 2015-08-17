from sleekxmpp import ClientXMPP
from xml.etree import cElementTree as ET
import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')

class PubSubClient(ClientXMPP):
	""" Handles PubSub by creating custom xml stanza and then sending it """

	def  __init__(self, node):
		self.node = node
		ClientXMPP.__init__(self,
			config.get('pubsub','pubsub_jid'), 
			config.get('pubsub','pubsub_password'))
		self.add_event_handler('session_start', self.send_pubsub_stanza)

	def send_pubsub_stanza(self):
		iq = self.handle_pubsub_iq()
		iq.send()

	def handle_pubsub_iq(self):
		pubsub_addr = config.get('pubsub','pubsub_addr')
		iq = self.make_iq_set(sub = self.create_xml_stanza(), ito=pubsub_addr)
		return iq

	def create_xml_stanza(self):
		pubsub = ET.Element('pubsub', xmlns='http://jabber.org/protocol/pubsub')
		publish = ET.SubElement(pubsub, 'publish', node=self.node) 
		item = ET.SubElement(publish, 'item')
		entry = ET.SubElement(item, 'entry')
		return pubsub

class PubSubService():
	def __init__(self, node):
		xmpp = PubSubClient(node)
		xmpp.register_plugin('xep_0030') # Service Discovery
		xmpp.register_plugin('xep_0004') # Data Forms
		xmpp.register_plugin('xep_0060') # PubSub
		xmpp.register_plugin('xep_0199') # XMPP Ping
		if xmpp.connect(('localhost', 5222)):
			xmpp.process(block=True)
			xmpp.disconnect()
			print("Done")
		else:
			print("Unable to connect.")