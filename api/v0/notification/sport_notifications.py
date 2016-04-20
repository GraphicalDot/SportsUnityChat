from abc import ABCMeta, abstractmethod, abstractproperty

class SportNotifications():
	__metaclass__ = ABCMeta

	# @abstractproperty	
	# def notification_dict(self):
	# 	raise NotImplementedError

	@abstractmethod
	def create_stanza():
		pass
 