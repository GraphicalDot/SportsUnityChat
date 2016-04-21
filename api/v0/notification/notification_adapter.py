from pubsub import PubSubNotificationService
from football_notifications import FootballNotifications 
from tennis_notifications import TennisNotifications
from cricket_notification import CricketNotifications

class NotificationAdapter(object):

	def __init__(self, event, sport_name):
		print "sport is %s " % sport_name
		if sport_name == "Football":
			sport_notification = FootballNotifications
		elif sport_name == "Tennis":
			sport_notification = TennisNotifications
		elif sport_name == "Cricket":
			sport_notification = CricketNotifications
		else:
			raise NotImplementedError
		self.sport_notification = sport_notification(event)

	def notify(self):
		notification = self.sport_notification.create_stanza()
		PubSubNotificationService(notification)

	def __getattr__(self, attr):
		return getattr(self.sport_notification, attr)