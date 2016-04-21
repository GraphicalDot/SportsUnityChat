from image import Image
class Group(object):
	def __init__(self, name):
		self.name = name

	def upload_dp(self, content):
		image = Image(self.name, content)
		image.version()
		image.handle_upload()

