from ConfigParser import ConfigParser
from IPython import embed
from notification_adapter import NotificationAdapter
from tornado.testing import AsyncHTTPTestCase
from xml.etree import cElementTree as ET
import api_v0_archive
import settings
import json
import unittest
import xmltodict
config = ConfigParser()
config.read('config.py')

_test_football_data =  {
	'league_id' : "league_id",
	'home_team'  : "home_team_name",
	'away_team' : "away_team_name",
	'match_date' : "date",
	'match_id' : "match_id",
	'home_team_score' : "home_team_score",
	'away_team_score' : "away_team_score",
	'match_status' : "match_status",
	'match_time' : "match_time"
}

_test_tennis_data = {
	"date": "2015-09-1",
	"final_score": "1  :  3",
	"match_staus": "Finished",
	"players": "Coric B. vs Nadal R.",
	"sets": [
		"3 - 6",
		"2 - 6",
		"6 - 4",
		"4 - 6"],
	"tournament": "ATP Singles: US Open"
}

_test_cricket_data = {
	"teams": "Aus Vs India",
	"status": "Aus won by 59 runs",
	"runs": "246",
	"wickets": "10",
	"time": "1441367561.578593",
	"overs": "45.3",
	42.2: "Starc to Mark Wood, 1 run, short delivery on the leg stump, Wood goes for the pull, mistimes it and it rolls to deep mid-wicket (Score after 42.2 Ov - 234)"
}

_xml_tennis_data_element = [
	"date",
	"final_score",
	"match_staus",
	"players",
	"sets",
	"tournament"
]

_xml_football_data_element = [
	"league_id" ,
	"home_team" ,
	"away_team" ,
	"match_id" ,
	"home_team_score" ,
	"away_team_score" ,
	"match_status" ,
	"match_time"
]

_xml_cricket_data_element = [
	"teams",
	"status",
	"runs",
	"wickets",
	"time",
	"overs",
	"Over42.2"
]



class PubSubServiceTest(AsyncHTTPTestCase):

	_publish_score = '/publish_score?sport=football&score'
	_new_event = '/new_event?name=BorussiaDortmund'

	def get_app(self):
		return api_v0_archive.make_app()

	def test_football_xml_creator(self):

		global _test_football_data

		notification = NotificationAdapter(_test_football_data, "Football")
		event_xml = notification.create_stanza()
		xml_dict = xmltodict.parse(ET.tostring(event_xml))

		assert xml_dict['pubsub']
		assert xml_dict['pubsub']['publish']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']['entry']
		assert xml_dict['pubsub']['publish']['item']['entry']['room_id']

		global _xml_football_data_element

		for xml_element in _xml_football_data_element:
			assert xml_dict['pubsub']['publish']['item']['entry'][xml_element]

	def test_tennis_xml_creator(self):
		global _test_football_data
		notification = NotificationAdapter(_test_tennis_data, "Tennis")
		event_xml = notification.create_stanza()
		xml_dict = xmltodict.parse(ET.tostring(event_xml))
		assert xml_dict['pubsub']
		assert xml_dict['pubsub']['publish']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']['entry']
		assert xml_dict['pubsub']['publish']['item']['entry']['room_id']

		global _xml_tennis_data_element
		for xml_element in _xml_tennis_data_element:
			assert xml_dict['pubsub']['publish']['item']['entry'][xml_element]
			if xml_element == 'sets':
				assert type(xml_dict['pubsub']['publish']['item']['entry'][xml_element]['value']) == list

	def test_cricket_xml_creator(self):
		notification = NotificationAdapter(_test_cricket_data, "Cricket")
		event_xml = notification.create_stanza()
		xml_dict = xmltodict.parse(ET.tostring(event_xml))
		assert xml_dict['pubsub']
		assert xml_dict['pubsub']['publish']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']['entry']
		assert xml_dict['pubsub']['publish']['item']['entry']['room_id']

		for xml_element in _xml_cricket_data_element:
			assert xml_dict['pubsub']['publish']['item']['entry'][xml_element]


class SportRequestsTest(AsyncHTTPTestCase):
	_cricket_notifications = '/cricket_notifications' + '?apk_version=v0.1&udid=TEST@UDID'
	_tennis_notifications = '/tennis_notifications' + '?apk_version=v0.1&udid=TEST@UDID'
	_football_notifications = '/football_notifications' + '?apk_version=v0.1&udid=TEST@UDID'

	def get_app(self):
		return api_v0_archive.make_app()

	def test_football_request(self):
		global _test_football_data
		self.http_client.fetch(self.get_url(self._football_notifications),
							   self.stop,
							   method = 'POST',
							   body = json.dumps(_test_football_data)
							   )
		response = self.wait(timeout = 50)
		self.assertEqual(response.code, settings.STATUS_200)

	def test_tennis_requests(self):
		global _test_tennis_data
		self.http_client.fetch(self.get_url(self._tennis_notifications),
							   self.stop,
							   method = 'POST',
							   body = json.dumps(_test_tennis_data)
							   )
		response = self.wait(timeout = 50)
		self.assertEqual(response.code, settings.STATUS_200)

	def test_cricket_requests(self):
		self.http_client.fetch(self.get_url(self._cricket_notifications), self.stop, method='POST', body=json.dumps(_test_cricket_data))
		response = self.wait(timeout=50)
		self.assertEquals(response.code, settings.STATUS_200)


if __name__ == '__main__':
	unittest.main()
