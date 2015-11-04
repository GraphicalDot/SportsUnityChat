from sport_notifications import SportNotifications
from xml.etree import cElementTree as ET
import time


class CricketNotifications(SportNotifications):
	def __init__(self,value_dict):
		self.event_dict = value_dict

	def create_stanza(self):
		pubsub = ET.Element('pubsub', xmlns='http://jabber.org/protocol/pubsub')
		publish = ET.SubElement(pubsub, 'publish', node="Cricket")
		item = ET.SubElement(publish, 'item')
		entry = ET.SubElement(item, 'entry')
		for key, value in self.event_dict.iteritems():
			if isinstance(key, int) or isinstance(key, float):
				key = 'Over' + str(key)
			key_tag = ET.SubElement(entry, key)
			key_tag.text = value
		room_id = str.strip(str(self.event_dict['teams'])).replace(" ", "") + str(int(time.time()))
		room_id_tag = ET.SubElement(entry, "room_id")
		room_id_tag.text = room_id
		return pubsub


# _test_cricket_data = {
# 	"teams": "Aus Vs India",
# 	"status": "Aus won by 59 runs",
# 	"runs": "246",
# 	"wickets": "10",
# 	"time": "1441367561.578593",
# 	"overs": "45.3",
# 	"42.2": "Starc to Mark Wood, 1 run, short delivery on the leg stump, Wood goes for the pull, mistimes it and it rolls to deep mid-wicket (Score after 42.2 Ov - 234)"
# }
