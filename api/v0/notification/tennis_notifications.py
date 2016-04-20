from sport_notifications import SportNotifications
from xml.etree import cElementTree as ET
import time

class TennisNotifications(SportNotifications):
	def __init__(self, event_dict):
		self.event_dict = event_dict

	def create_stanza(self):
		pubsub = ET.Element('pubsub', xmlns='http://jabber.org/protocol/pubsub')
		publish = ET.SubElement(pubsub, 'publish', node="Tennis") 
		item = ET.SubElement(publish, 'item')
		entry = ET.SubElement(item, 'entry')
		for key, value in self.event_dict.iteritems():
			if type(value) == list:
				sets = ET.SubElement(entry, key, type = 'array')
				for element in value:
					array_element = ET.SubElement(sets, 'value')
					array_element.text = element
			else:
				key_tag = ET.SubElement(entry, key)
				key_tag.text = value
		room_id = str.strip(str(self.event_dict['players'])).replace(" ","").replace(".","")\
			+ str(int(time.time()))
		room_id_tag = ET.SubElement(entry, "room_id")
		room_id_tag.text = room_id
		return pubsub

# _test_tennis_data = {
#     "date": "2015-09-1",
#     "final_score": "1  :  3",
#     "match_staus": "Finished",
#     "players": "Coric B. vs Nadal R.",
#     "sets": [
#       "3 - 6",
#       "2 - 6",
#       "6 - 4",
#       "4 - 6"],
#     "tournament": "ATP Singles: US Open"
# 	}
