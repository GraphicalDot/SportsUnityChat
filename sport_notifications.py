from abc import ABCMeta, abstractmethod, abstractproperty

class SportNotifications(ABCMeta):
	__metaclass__ = ABCMeta

	@abstractproperty	
	def notification_dict(sellf):
		pass

	@abstractmethod
	def get_stanza(self, notification_dict):
		pass

	@abstractmethod
	def create_stanza():
		pass

class NotificationAdapter(object):

	def __init__(self, notification_dict, sport_name):
		self.sport = sport_name
		self.sport.notification_dict = notification_dict

	def __getattr__(self, attr):
		return getattr(self.sport, attr) 