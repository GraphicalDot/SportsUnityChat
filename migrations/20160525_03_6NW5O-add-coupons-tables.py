"""
Add coupons tables
"""

from yoyo import step

__depends__ = {'20160525_02_wURid-add-table-for-coupons'}

steps = [
    step(" CREATE TABLE coupons (code TEXT PRIMARY KEY, created_at TIMESTAMP DEFAULT now(), coupon_limit integer NOT NULL, used_count integer NOT NULL DEFAULT 0 ) ")
]
