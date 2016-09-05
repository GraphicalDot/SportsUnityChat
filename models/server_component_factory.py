import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')
from server_component import ServerComponent
import time
from rq import Queue
from redis import Redis

class ServerComponentFactory(object):
	def __init__(self, jid, password):
		self.jid = jid
		self.password = password
		redis_conn = Redis()
		self.queue = Queue(connection=redis_conn)

	
	def send(self, message):
		self.queue.enqueue(start_asynchronous_sending, self.jid, self.password, message)

def start_asynchronous_sending(jid, password, message):
	ServerComponent(jid, password, message)
