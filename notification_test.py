from pubsub import PubSubNotificationClient
from tornado.testing import AsyncHTTPTestCase, LogTrapTestCase
import unittest
from global_func import QueryHandler, S3Handler
import api_v0_archive
import facebook
from IPython import embed 
import json
import os, sys
import requests
import psycopg2
import psycopg2.extras
import xmltodict, json
from xml.etree import cElementTree as ET
from ConfigParser import ConfigParser
from notification_adapter import NotificationAdapter
from football_notifications import FootballNotifications
config = ConfigParser()
config.read('config.py')

class PubSubServiceTest(AsyncHTTPTestCase):

	_publish_score = '/publish_score?sport=football&score'
	_new_event = '/new_event?name=BorussiaDortmund'
	_cricket_commentary = '/cricket_notifications'
	_tennis_notifications = '/tennis_notifications'
	_football_notifications = '/football_notifications'
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

	_sports = ["Football"]

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
	
	def get_app(self):
		return api_v0_archive.make_app()
	
	def test_football_xml_creator(self):

		notification = NotificationAdapter(self._test_football_data, sport)
		event_xml = notification.create_stanza()
		xml_dict = xmltodict.parse(ET.tostring(event_xml))
		
		assert xml_dict['pubsub']
		assert xml_dict['pubsub']['publish']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']['entry']
		assert xml_dict['pubsub']['publish']['item']['entry']['room_id']
		
		for xml_element in self._xml_football_data_element:
			assert xml_dict['pubsub']['publish']['item']['entry'][xml_element]

if __name__ == '__main__':
	unittest.main()