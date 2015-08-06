#TO-DO non blocking database wrapper
import psycopg2
import psycopg2.extras
import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')

class QueryHandler:
	@classmethod
	def get_connection(cls):
		connection = psycopg2.connect("dbname=%s host=%s user=%s password=%s" 
			% (config.get('database','database') ,
			config.get('database','host') ,
			config.get('database','user') ,
			config.get('database','password'))
		)
		return connection

	@classmethod
	def get_results(cls, query, variables):
		connection = cls.get_connection()
		cursor = connection.cursor(cursor_factory=psycopg2.extras.RealDictCursor)
		print(cursor.mogrify(query, variables))
		cursor.execute(query, variables)
		results = cursor.fetchall()
		connection.commit()
		cursor.close()
		return results

	@classmethod
	def execute(cls, query, variables):
		connection = cls.get_connection()
		cursor = connection.cursor()
		print(cursor.mogrify(query, variables))
		cursor.execute(query, variables)
		connection.commit()
		cursor.close()