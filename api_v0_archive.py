import tornado.ioloop
import tornado.web
import psycopg2

class QueryHandler:
	@classmethod
	def get_results(cls, query, variables):
		connection = psycopg2.connect("dbname=development host=localhost user=development password=development")
		cursor = connection.cursor()
		return cursor.execute(query, variables)

class ArchiveAcessHandler(tornado.web.RequestHandler):
    def get(self):
    	from_timestamp = self.get_arguments("from", True) 
        to_timestamp = self.get_arguments("to", True)
        skip = self.get_arguments("skip", True) or 0
    	limit = self.get_arguments("limit", True) or 100
        response = {}
        response['version'] = 0.1
        try:
        	response['info'] = QueryHandler.get_results(" SELECT txt FROM archives WHERE timestamp > (%s) AND timestamp < (%s) OFFSET (%s) LIMIT (%s); ",(from_timestamp, to_timestamp, skip, limit, ))
        	response['status'] = 200
        except Exception, e:
            response['info'] = " An error has occured. Developers should be notified "
            response['status'] = 500
        self.write(response)

application = tornado.web.Application([
    (r"/messages", ArchiveAcessHandler),
], 
autoreload = True)

if __name__ == "__main__":
    application.listen(3000)
    tornado.ioloop.IOLoop.current().start()