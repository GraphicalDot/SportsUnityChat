from sleekxmpp import ClientXMPP
from xml.etree import cElementTree as ET
import ConfigParser
from IPython import embed
config = ConfigParser.ConfigParser()
config.read('config.py')

class PubSubClient(ClientXMPP):
	""" Handles PubSub by creating custom xml stanza and then sending it """

	def  __init__(self, node, message):
		self.node = node
		self.message = message
		ClientXMPP.__init__(self,
			config.get('pubsub','pubsub_jid'), 
			config.get('pubsub','pubsub_password'))
		self.add_event_handler('session_start', self.start)

	def start(self, event):
		iq = self.handle_pubsub_iq()
		try:
			message = iq.send(block=True, now=True)
			print("Message sent")
			self.disconnect(reconnect=False, wait=None)
		except IqError as e:
			logging.error("Could not send")
			self.disconnect(reconnect=False)
		except IqTimeout:
			logging.error("No response from server.")

	def handle_pubsub_iq(self):
		pubsub_addr = config.get('pubsub','pubsub_addr')
		iq = self.make_iq_set(sub = self.create_xml_stanza(), ito=pubsub_addr)
		return iq

	def create_xml_stanza(self):
		pubsub = ET.Element('pubsub', xmlns='http://jabber.org/protocol/pubsub')
		publish = ET.SubElement(pubsub, 'publish', node=self.node) 
		item = ET.SubElement(publish, 'item')
		entry = ET.SubElement(item, 'entry')
		entry.text = self.message
		return pubsub

class PubSubService():
	def __init__(self, 
			node=config.get('pubsub', 'node'), 
			message=config.get('pubsub', 'sample_message')):
		xmpp = PubSubClient(node, message)
		if xmpp.connect(('localhost', 5222)):
			xmpp.process(block=True)
			print("Done")
		else:
			print("Unable to connect.")

if __name__ == '__main__':
	xmpp = PubSubClient(node=config.get('pubsub', 'node'), 
		message=config.get('pubsub', 'sample_message'))
	xmpp.register_plugin('xep_0030') 
	xmpp.register_plugin('xep_0004') 
	xmpp.register_plugin('xep_0066') 
	xmpp.register_plugin('xep_0077') 
	if xmpp.connect(('localhost', 5222)):
		xmpp.process(block=True)
		print("Done")
	else:
		print("Unable to connect.")
