import ConfigParser
import base64
config = ConfigParser.ConfigParser()
config.read('config.py')
from s3_image import S3Image
from s3_object import S3Object
class Dp(object):
	"""
	Represents a display picture class
	Methods include
	1. Uploading dp
	2. Get dp
	3. checking for existence of dp
	"""
	def __init__(self, jid):
		self.jid = jid
		self.dp_bucket = str.strip(config.get('amazon', 'dp_bucket_name'))
		self.acl = str.strip(config.get('amazon', 'dp_objects_acl'))

	def upload_dp(self, content, is_b64_encoded = True):
		if is_b64_encoded:
			content = base64.b64decode(content)
		image = S3Image(self.jid, self.dp_bucket, content = content)
		image.version()
		image.handle_upload()

	def get_dp_version(self, version):
		if not version in ["L", "S"]:
			raise BadInfoSuppliedError("photo version")
		key = self.jid + "/" + version + ".jpg"
		return S3Object(name = key, bucket_name = self.dp_bucket).download()

	def exists(self, version):
		if not version in ["L", "S"]:
			raise BadInfoSuppliedError("photo version")
		key = self.jid + "/" + version + ".jpg"
		return S3Object(name = key, bucket_name = self.dp_bucket).exists()
		
