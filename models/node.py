import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')
from s3_image import S3Image
class Node(object):
	def __init__(self, jid):
		self.jid = jid
		self.dp_bucket = str.strip(config.get('amazon', 'dp_bucket_name'))

	def upload_dp(self, content):
		image = S3Image(self.jid, self.dp_bucket, content)
		image.version()
		image.handle_upload()

	def get_dp_version(self, version):
		if not version in ["L", "S"]:
			raise BadInfoSuppliedError("photo version")
		key = self.jid + "/" + version + ".jpg"
		return S3Image(self.jid, self.dp_bucket).get_file(key)