"""
Add properties to users_interest
"""

from yoyo import step

__depends__ = {}

steps = [
    step("ALTER TABLE users_interest ADD COLUMN properties json")
]
