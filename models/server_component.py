import sleekxmpp.componentxmpp

class ServerComponent(object):
	
	def __init__(self, jid, password, server, port) :
		self.xmpp = sleekxmpp.componentxmpp.ComponentXMPP(jid, password, server, port)
		self.xmpp.add_event_handler("session_start", self.handleXMPPConnected)
		self.xmpp.add_event_handler("message", self.message_received)

	def handleXMPPConnected(self, event) :
		raise NotImplementedError	

	def message_received(self, message):
		print message

	def send(self) :
		self.xmpp.connect()
		self.xmpp.process(block = False)