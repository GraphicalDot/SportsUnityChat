import settings
import threading
from common.funcs import S3
import copy

class S3Object(S3):

	def __init__(self, name, bucket_name, content = None):
		super(S3Object, self).__init__(bucket_name = bucket_name, name = name, content = content)
		self.name = name
		self.bucket_name = bucket_name
		self.content = content

	def handle_upload(self):
		threading.Thread(group = None, target = self.upload, name = None, args = ()).start()

	def upload(self):
		super(S3Object, self).upload()

	def delete(self):
		self.delete_key()

	def exists(self):
		return super(S3Object, self).check_exists()

	def download(self):
		return super(S3Object, self).get_file()
