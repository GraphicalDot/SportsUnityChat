import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')
from server_component import ServerComponent
import threading

class ServerComponentFactory(object):
	def __init__(self, jid, password):
		self.jid = jid
		self.password = password
	
	def send(self, message):
		threading.Thread(group = None, target = ServerComponent, name = None, args = (self.jid, self.password, message)).start()