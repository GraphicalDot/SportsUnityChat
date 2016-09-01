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

	
	def send_group_creation(self, message):
		self.queue.enqueue(start_asynchronous_group_creation_sending, self.jid, self.password, message)


	def send_user_addition(self, message):
		self.queue.enqueue(start_asynchronous_user_addition_sending, self.jid, self.password, message)


def start_asynchronous_group_creation_sending(jid, password, message):
	ServerComponent(jid, password, message)

def start_asynchronous_user_addition_sending(jid, password, message):
	ServerComponent(jid, password, message)
