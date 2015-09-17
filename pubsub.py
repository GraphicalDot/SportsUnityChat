from sleekxmpp import ClientXMPP
from xml.etree import cElementTree as ET
import ConfigParser
from IPython import embed
config = ConfigParser.ConfigParser()
config.read('config.py')

class PubSubNotificationClient(ClientXMPP):
	""" Handles PubSub by creating custom xml stanza and then sending it """

	def  __init__(self, notification):
		self.notification = notification
		ClientXMPP.__init__(self,
			config.get('pubsub','pubsub_jid'), 
			config.get('pubsub','pubsub_password'))
		self.add_event_handler('session_start', self.start)

	def start(self, event):
		iq = self.handle_pubsub_iq()
		print "sending: %s " % ET.tostring(self.notification)
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
		iq = self.make_iq_set(sub = self.notification, ito=pubsub_addr)
		return iq

class PubSubNotificationService():
	"""Acts as the interface to the xmpp pubsub service 
	including connecting and sending the notifications"""
	def __init__(self, 
			notification_message=config.get('pubsub', 'sample_message')):
		xmpp = PubSubNotificationClient(notification_message)
		if xmpp.connect(('localhost', 5222)):
			xmpp.process(block=True)
			print("Done")
		else:
			print("Unable to connect.")

if __name__ == '__main__':
	xmpp = PubSubNotificationClient(node=config.get('pubsub', 'node'), 
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
