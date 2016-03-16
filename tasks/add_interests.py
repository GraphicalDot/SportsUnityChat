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
		query = " INSERT INTO interest_type (interest_type) VALUES (%s);"
		variables = (interest_type,)
		QueryHandler.execute(query, variables)
	except IntegrityError, e:
		pass

def add_interest(name, interest_id, interest_type):
	try:
		query = " INSERT INTO interest (interest_name, interest_uid, interest_type_id) "\
		+ " VALUES (%s, %s, (SELECT interest_type_id FROM interest_type WHERE interest_type = %s)); "
		variables = (name, interest_id, interest_type,)
		QueryHandler.execute(query, variables)
	except IntegrityError, e:
		pass

def set_player_names():
	response = json.loads(requests.get(settings.PLAYERS_NAME_URL).content)['data']
	interest_type = "player"
	for player in response:
		assert player["name"]
		assert player["player_id"]
		add_interest(player["name"], player["player_id"], interest_type)

def handle_players():
	set_interest_type("player")	
	set_player_names()

def run_tasks():
	handle_players()

if __name__ == '__main__':
	run_tasks()