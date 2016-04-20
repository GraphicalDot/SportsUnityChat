import importlib
api_versions = ["v0"]
urls = []
for version in api_versions:
	api_version_url_file = importlib.import_module("api.{}.routes".format(version))
	urls += api_version_url_file.urls