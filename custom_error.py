import settings
class BadAuthentication(Exception):
	log_message = settings.BAD_AUTHENTICATION_ERROR
	