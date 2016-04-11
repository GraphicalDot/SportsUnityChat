import requests
import sys
from os import path
sys.path.append( path.dirname( path.dirname( path.abspath(__file__) ) ) )
from global_func import QueryHandler
import settings
from psycopg2 import IntegrityError
import json

def set_interest_type(interest_type):
	try:
		query = " INSERT INTO interest_type (interest_type_name) VALUES (%s);"
		variables = (interest_type,)
		QueryHandler.execute(query, variables)
	except IntegrityError, e:
		pass

def add_interest(name, interest_id, interest_type):
	try:
		try:
			print "Adding {} with id {} of type {}".format(name, interest_id, interest_type)
		except UnicodeEncodeError, e:
			pass
		query = " INSERT INTO interest (interest_name, interest_id, interest_type_id) "\
		+ " VALUES (%s, %s, (SELECT interest_type_id FROM interest_type WHERE interest_type_name = %s)); "
		variables = (name, interest_id, interest_type,)
		QueryHandler.execute(query, variables)
	except IntegrityError, e:
		print e
		pass
	except Exception, e:
		from IPython import embed
		embed()

def set_player_names():
	response = json.loads(requests.get(settings.PLAYERS_NAME_URL).content)['data']
	interest_type = settings.PLAYER_INTEREST_TYPE_NAME
	for player in response:
		if player.get("player_id", None):
			add_interest(player["name"], player["player_id"], interest_type)

def set_cricket_team_names():
	response = json.loads(requests.get(settings.CRICKET_TEAM_NAME_URL).content)['data']
	interest_type = settings.TEAM_INTEREST_TYPE_NAME
	for team in response:
		if team.get("team_id", None):
			add_interest(team["team_id"], team["team_name"], interest_type)

def set_football_team_names():
	response = json.loads(requests.get(settings.FOOTBALL_TEAM_NAME_URL).content)['data']
	interest_type = settings.TEAM_INTEREST_TYPE_NAME
	for team in response:
		if team.get("team_id", None):
			add_interest(team["team_id"], team["team_name"], interest_type)

def handle_players():
	set_interest_type(settings.PLAYER_INTEREST_TYPE_NAME)	
	set_player_names()

def handle_teams():
	set_interest_type(settings.TEAM_INTEREST_TYPE_NAME)
	set_cricket_team_names()
	set_football_team_names()

def run_tasks():
	handle_players()
	handle_teams()

if __name__ == '__main__':
	run_tasks()