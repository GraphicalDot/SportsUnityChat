import base64
import ConfigParser
from s3_object import S3Object
config = ConfigParser.ConfigParser()
import settings
config.read('config.py')
import random

class Node(object):
	"""
	This class defines a generic node. A node is anything which can have a disctinct identity like
	1. group
	2. user
	3. discussion
	"""
	def get_random_avatar(self):
		key = str(random.randint(1, 1000))
		bucket = config.get('amazon', 'random_avatar_bucket')
		return base64.b64encode(S3Object(key, bucket).download())

