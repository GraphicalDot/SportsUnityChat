"""
Add column referral_code to users
"""

from yoyo import step

__depends__ = {'20160415_03_0jzkj-drop-seperate-token-columns-in-users', '20160512_01_y9vg9-change-show-location-datatype-and-data'}

steps = [
    step("ALTER TABLE users ADD COLUMN referral_code TEXT UNIQUE")
]
