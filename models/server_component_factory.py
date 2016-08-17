import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')
from server_component import ServerComponent

class ServerComponentFactory(object):
	
	_instance = None

	def __new__(cls, *args, **kwargs):
		if not cls._instance:
			cls._instance = super(ServerComponentFactory, cls).__new__(cls, *args, **kwargs)
		return cls._instance

	def __init__(self):
		self.server = config.get('server_component', 'server')
		self.port = config.get('server_component', 'port')
		self.components = {}

	def get_server_component(self, jid, password):
		if not self.components.get(jid, None):
			self.components[jid] = ServerComponent(jid, password, self.server, self.port)
		return self.components[jid]
