from common.funcs import QueryHandler, S3, merge_dicts, send_message
class Interest(object):
	def __init__(self, name, iid):
		self.name = name
		self.iid = iid

	def create(self):
		query = "INSERT INTO interest (interest_id, interest_name) VALUES (%s, %s);"
		variables = (self.iid, self.name)
		QueryHandler.execute(query, variables)

	def delete(self):
		query = " DELETE FROM interest WHERE interest_id = %s;"
		variables = (self.iid, )
		QueryHandler.execute(query, variables)		