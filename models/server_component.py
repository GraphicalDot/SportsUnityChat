import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')
import sleekxmpp.componentxmpp
import time
import threading
class ServerComponent(object):

	def __init__(self, jid, password, server, port):

		self.message_queue = []

		self.xmpp = sleekxmpp.componentxmpp.ComponentXMPP(jid, password, server, port)

		self.xmpp.add_event_handler("session_start", self.handle_connected)
		self.xmpp.add_event_handler("disconnected", self.handle_disconnected)

		self.threaded_connect()

	def handle_connected(self):
		for message in self.message_queue:
			if self.connected:
				self.send_raw(self.message_queue.pop())

	def handle_disconnected(self):
		time.sleep(5)
		self.threaded_connect()

	def connect(self):
		self.xmpp.connect()
		self.xmpp.process(block = False)

	def threaded_connect(self):
		threading.Thread(group = None, target = self.connect, name = None, args = ()).start()

	def send_raw(self, message):
		if self.xmpp.state.current_state() == 'connected':
			self.handle_connected()
			self.xmpp.send_raw(message, now = True)
		else:
			self.message_queue.append(message)
