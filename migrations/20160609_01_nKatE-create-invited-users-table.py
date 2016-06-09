"""
 Create invited_users table
"""

from yoyo import step

__depends__ = {}

steps = [

    step(" CREATE TABLE invited_users (phone_number TEXT NOT NULL, created_at TIMESTAMP DEFAULT NOW(), gateway_response JSON) ")
]
