from sport_notifications import SportNotifications
from xml.etree import cElementTree as ET
import tornado
import time

class FootballNotifications(SportNotifications):
	def __init__(self, event_dict):
		self.event_dict = event_dict

	def create_stanza(self):
		pubsub = ET.Element('pubsub', xmlns='http://jabber.org/protocol/pubsub')
		publish = ET.SubElement(pubsub, 'publish', node="Football") 
		item = ET.SubElement(publish, 'item')
		entry = ET.SubElement(item, 'entry')
		for key, value in self.event_dict.iteritems():
			key_tag = ET.SubElement(entry, key)
			key_tag.text = value
		room_id = str.strip(str(self.event_dict['match_id'])) + str(int(time.time()))
		room_id_tag = ET.SubElement(entry, "room_id")
		room_id_tag.text = room_id
		return pubsub


#{'league_id' : league_id,
#'home_team'  : home_team_name,
#'away_team' : away_team_name,
#'match_id' : match id,
#'home_team_score' : home team score,
#'away_team_score' : away team score,
#'match_status' : match status,
#'match_time' : match time}

