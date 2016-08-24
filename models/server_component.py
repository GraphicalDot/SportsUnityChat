import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')
import sleekxmpp.componentxmpp
import time
import threading
class ServerComponent(object):

	def __init__(self, jid, password, message):
		self.server = config.get('server_component', 'server')
		self.port = config.get('server_component', 'port')
		self.message = message

		self.xmpp = sleekxmpp.componentxmpp.ComponentXMPP(jid, password, self.server, self.port)

		self.xmpp.add_event_handler("session_start", self.handle_connected)

		self.connect()

	def handle_connected(self, event = None):
		self.xmpp.send_raw(self.message, now = True)
		self.xmpp.disconnect()

	def connect(self):
		self.xmpp.connect()
		self.xmpp.process(block = False)