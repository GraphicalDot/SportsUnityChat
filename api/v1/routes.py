import handlers
urls = [
      (r"/v1/set_location_privacy", handlers.LocationPrivacyHandler),
      (r"/v1/set_user_interests", handlers.UserInterestHandler),
      (r"/v1/group_subscription_to_commentary", handlers.SubscribeToCommentary),
]