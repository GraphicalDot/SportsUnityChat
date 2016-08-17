"""
Add new table for users_poll_responses, articles_discussions and discussions_users
"""

from yoyo import step

__depends__ = {'20160803_01_kErzw-add-curated-articles-tables', '20160802_02_OVZsZ-add-table-for-user-poll'}

steps = [
    step("CREATE TABLE articles_discussions (discussion_id TEXT PRIMARY KEY, article_id INTEGER REFERENCES articles ON DELETE CASCADE, created_at TIMESTAMP DEFAULT now());"),
    step("CREATE TABLE discussions_users (discussion_id TEXT REFERENCES articles_discussions ON DELETE CASCADE, username TEXT REFERENCES users ON DELETE CASCADE, created_at TIMESTAMP DEFAULT now());")
]
