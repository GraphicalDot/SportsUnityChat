"""
 Add tables for limiting parameters in case of group creation in chat api
"""

from yoyo import step

__depends__ = {}

steps = [
	step("CREATE TABLE min_group_users(count INTEGER NOT NULL, created_at TIMESTAMP DEFAULT now());"),
	step("CREATE TABLE max_group_users(count INTEGER NOT NULL, created_at TIMESTAMP DEFAULT now());"),
	step("CREATE TABLE max_group_ratio(count INTEGER NOT NULL, created_at TIMESTAMP DEFAULT now());"),
	step("CREATE TABLE min_group_users_ratio(count INTEGER NOT NULL, created_at TIMESTAMP DEFAULT now());")
]
