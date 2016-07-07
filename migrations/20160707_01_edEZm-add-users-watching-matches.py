"""

"""

from yoyo import step

__depends__ = {}

steps = [
    step(" CREATE TABLE users_watching_matches (username TEXT REFERENCES users ON DELETE CASCADE, match_id TEXT NOT NULL, created_at TIMESTAMP DEFAULT now());")
]
