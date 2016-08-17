import settings


class BadAuthentication(Exception):
    log_message = settings.BAD_AUTHENTICATION_ERROR


class InvalidBucketRequest(Exception):
    log_message = settings.INVALID_BUCKET_ERROR


class KeyAlreadyExists(Exception):
    log_message = settings.KEY_ALREADY_EXISTS


class BadInfoSuppliedError(Exception):
    log_message = settings.BAD_INFO_ERROR
    def __init__(self, argument):
        self.log_message = self.log_message.format(argument)


class DuplicateKeyError(Exception):
    log_message = settings.DUPLICATE_KEY_ERROR
    def __init__(self, argument):
        self.log_message = self.log_message.format(argument)
