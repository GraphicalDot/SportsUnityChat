"""
Add default values for tables related to users discussions parameters
"""

from yoyo import step

__depends__ = {'20160808_02_j3Wv3-add-tables-for-limiting-parameters-in-case-of-chat-api'}

steps = [
    step(" WITH upsert AS (UPDATE min_discussion_users SET count = 2 RETURNING * ) INSERT INTO min_discussion_users (count) SELECT  2 WHERE NOT EXISTS (SELECT * FROM upsert)"),
    step(" WITH upsert AS (UPDATE max_discussion_users SET count = 8 RETURNING * ) INSERT INTO min_discussion_users (count) SELECT  2 WHERE NOT EXISTS (SELECT * FROM upsert)"),
    step(" WITH upsert AS (UPDATE max_discussion_ratio SET count = 10 RETURNING * ) INSERT INTO min_discussion_users (count) SELECT  2 WHERE NOT EXISTS (SELECT * FROM upsert)"),
    step(" WITH upsert AS (UPDATE min_discussion_users_ratio SET count = 0.01 RETURNING * ) INSERT INTO min_discussion_users (count) SELECT  2 WHERE NOT EXISTS (SELECT * FROM upsert)")
]
