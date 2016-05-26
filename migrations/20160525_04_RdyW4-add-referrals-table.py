"""
Add referrals table
"""

from yoyo import step

__depends__ = {}

steps = [
    step("CREATE TABLE referrals (username TEXT PRIMARY KEY REFERENCES users ON DELETE CASCADE , created_at TIMESTAMP DEFAULT now(), referred_by TEXT NOT NULL)")
]
