import handlers
urls = [
      (r"/v1/set_location_privacy", handlers.LocationPrivacyHandler),
      (r"/v1/set_user_interests", handlers.UserInterestHandler),
      (r"/v1/set_user_watching_match", handlers.SetUserWatchingMatchHandler),
      (r"/v1/delete_user_watching_match", handlers.DeleteUserWatchingMatchHandler),
      (r"/v1/get_users_watching_matches", handlers.GetUserWatchingMatchHandler),
]