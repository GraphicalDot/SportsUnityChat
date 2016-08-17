import settings
class BadAuthentication(Exception):
	log_message = settings.BAD_AUTHENTICATION_ERROR

class BadInfoSuppliedError(Exception):
	log_message = settings.BAD_INFO_ERROR
	def __init__(self, argument):
		self.log_message = self.log_message.format(argument)

class InternalServerError(Exception):
	log_message = settings.INTERNAL_SERVER_ERROR
