import settings
import threading
from s3_object import S3Object
import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')

class UserMedia(S3Object):

	def __init__(self, name, content = None):
		self.bucket_name = str.strip(config.get('amazon', 'user_media_bucket_name'))
		self.acl = str.strip(config.get('amazon', 'user_media_acl'))
		super(UserMedia, self).__init__(bucket_name = self.bucket_name, name = name, content = content)
		self.name = name
		self.content = content

	def handle_upload(self):
		threading.Thread(group = None, target = self.upload, name = None, args = ()).start()

	def upload(self):
		super(UserMedia, self).upload()

	def delete(self):
		self.delete_key()

	def download(self):
		return super(UserMedia, self).download()

	def exists(self):
		return super(UserMedia, self).check_exists()
