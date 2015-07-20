import tornado.ioloop
import tornado.web
import psycopg2
from tornado.log import enable_pretty_logging

class QueryHandler:
	@classmethod
	def get_results(cls, query, variables):
		connection = psycopg2.connect("dbname=test host=localhost user=test password=test")
		cursor = connection.cursor()
		return cursor.execute(query, variables)

class ArchiveAcessHandler(tornado.web.RequestHandler):
    def get(self):
    	from_timestamp = str(self.get_arguments("from")) or 0
        to_timestamp = str(self.get_arguments("to")) or 345
        print(from_timestamp)
        print(to_timestamp)
        skip = self.get_arguments("skip", True) or 0
    	limit = self.get_arguments("limit", True) or 100
        response = {}
        response['version'] = 0.1
        try:
        	response['info'] = QueryHandler.get_results(" SELECT txt FROM archive WHERE timestamp > %s AND timestamp < %s OFFSET %s LIMIT %s; ",(from_timestamp, to_timestamp, skip, limit,))
        	response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)

application = tornado.web.Application([
    (r"/messages", ArchiveAcessHandler),
], 
autoreload = True,
)

if __name__ == "__main__":
    enable_pretty_logging()
    application.listen(3000)
    tornado.ioloop.IOLoop.current().start()