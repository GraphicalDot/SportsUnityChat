"""
Add new table for users_poll_responses, articles_groups and groups_users
"""

from yoyo import step

__depends__ = {'20160802_01_d4s6I-add-articles-table', '20160802_02_OVZsZ-add-table-for-user-poll'}

steps = [
    step("CREATE TABLE articles_groups (group_id TEXT PRIMARY KEY, article_id INTEGER REFERENCES articles ON DELETE CASCADE, created_at TIMESTAMP DEFAULT now());"),
    step("CREATE TABLE groups_users (group_id TEXT REFERENCES articles_groups ON DELETE CASCADE, username TEXT REFERENCES users ON DELETE CASCADE, created_at TIMESTAMP DEFAULT now());")
]
