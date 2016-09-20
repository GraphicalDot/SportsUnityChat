import wand.image 
import base64
import settings
import threading
from s3_object import S3Object
import copy

class S3Image(object):

	def __init__(self, name, bucket_name, content = None):
		self.name = name
		self.bucket_name = bucket_name
		self.versions = []
		self.content = content

	def version(self):
		image = wand.image.Image(blob = self.content)

		large_version_name = str(self.name) + "/L" + ".jpg"
		image.resize(settings.DP_L_WIDTH, settings.DP_L_HEIGHT)
		self.versions.append({large_version_name: image.make_blob(format='jpg')})

		small_version_name = str(self.name) + "/S" + ".jpg"
		image.resize(settings.DP_S_WIDTH, settings.DP_S_HEIGHT)
		self.versions.append({small_version_name: image.make_blob(format='jpg')})
		
	def handle_upload(self):
		for idx, val in enumerate(self.versions):
			threading.Thread(group = None, target = self.upload_to_s3, name = None, args = (idx, )).start()

	def upload_to_s3(self, index):
		for name, content in self.versions[index].iteritems():
			S3Object(bucket_name = self.bucket_name, name = name, content = content).upload()
