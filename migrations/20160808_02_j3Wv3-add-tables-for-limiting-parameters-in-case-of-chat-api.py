"""
 Add tables for limiting parameters in case of discussion creation in chat api
"""

from yoyo import step

__depends__ = {}

steps = [
	step("CREATE TABLE min_discussion_users(count INTEGER NOT NULL, created_at TIMESTAMP DEFAULT now());"),
	step("CREATE TABLE max_discussion_users(count INTEGER NOT NULL, created_at TIMESTAMP DEFAULT now());"),
	step("CREATE TABLE max_discussion_ratio(count INTEGER NOT NULL, created_at TIMESTAMP DEFAULT now());"),
	step("CREATE TABLE min_discussion_users_ratio(count INTEGER NOT NULL, created_at TIMESTAMP DEFAULT now());")
]
