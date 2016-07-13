--
-- ejabberd, Copyright (C) 2002-2015   ProcessOne
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--                         
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--

CREATE TABLE users (
    username text PRIMARY KEY,
    "password" text NOT NULL,
    phone_number text UNIQUE NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    lat double precision,
    lng double precision,
    fb_id bigint,
    fb_name text,
    last_seen text ,
    resume_id text,
    device_token text,
    token_type char,
    device_id text, 
    is_available boolean DEFAULT FALSE,
    is_banned boolean DEFAULT FALSE,
    show_location boolean,
    name text,
    referral_code text
);
CREATE INDEX i_users_username ON users USING btree (username);



CREATE TABLE matches(
    id text UNIQUE NOT NULL PRIMARY KEY,
    name text NOT NULL
);

 CREATE TABLE coupons (
    code TEXT PRIMARY KEY, 
    created_at TIMESTAMP DEFAULT now(), 
    coupon_limit integer NOT NULL,
    used_count integer NOT NULL DEFAULT 0 
);

CREATE TABLE referrals (
    username TEXT PRIMARY KEY REFERENCES users ON DELETE CASCADE , 
    created_at TIMESTAMP DEFAULT now(), 
    referred_by TEXT NOT NULL
)

CREATE TABLE notifications (
    id SERIAL PRIMARY KEY, 
    notification text,
    apns_response text,
    gcm_response text,
    created_at TIMESTAMP DEFAULT now(),
    match_id text
);

CREATE TABLE users_matches(
    username text REFERENCES users ON DELETE CASCADE,
    match_id text REFERENCES matches ON DELETE CASCADE,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_users_matches_match_id ON users_matches USING btree (match_id, username);

-- To support SCRAM auth:
-- ALTER TABLE users ADD COLUMN serverkey text NOT NULL DEFAULT '';
-- ALTER TABLE users ADD COLUMN salt text NOT NULL DEFAULT '';
-- ALTER TABLE users ADD COLUMN iterationcount integer NOT NULL DEFAULT 0;

CREATE EXTENSION cube;
CREATE EXTENSION earthdistance;
CREATE INDEX location_index on users USING gist(ll_to_earth(lat, lng));
CREATE OR REPLACE FUNCTION isnumeric(text) RETURNS BOOLEAN AS $$
DECLARE x NUMERIC;
BEGIN
    x = $1::NUMERIC;
    RETURN TRUE;
EXCEPTION WHEN others THEN
    RETURN FALSE;
END;
$$
STRICT
LANGUAGE plpgsql IMMUTABLE;


CREATE FUNCTION array_intersect(anyarray, anyarray)
  RETURNS anyarray
  language sql
as $FUNCTION$
    SELECT ARRAY(
        SELECT UNNEST($1)
        INTERSECT
        SELECT UNNEST($2)
    );
$FUNCTION$;


CREATE TABLE registered_users (
    authorization_code text ,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    phone_number text NOT NULL, 
    expiration_time bigint,
    gateway_response text
);
CREATE TABLE interest_type (
    interest_type_id SERIAL PRIMARY KEY ,
    interest_type_name text UNIQUE NOT NULL
);

CREATE TABLE interest (
    interest_id text PRIMARY KEY ,
    interest_name text NOT NULL,
    interest_type_id integer REFERENCES interest_type
);




CREATE TABLE users_interest (
    interest_id text REFERENCES interest ON DELETE CASCADE,
    username text REFERENCES users ON DELETE CASCADE,
    PRIMARY KEY (interest_id, username)
);

CREATE INDEX i_users_interest_username ON users_matches USING btree (username);

CREATE TABLE last (
    username text PRIMARY KEY,
    seconds text NOT NULL,
    state text NOT NULL
);


CREATE TABLE rosterusers (
    username text NOT NULL,
    jid text NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text,
    "type" text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers USING btree (username, jid);
CREATE INDEX i_rosteru_username ON rosterusers USING btree (username);
CREATE INDEX i_rosteru_jid ON rosterusers USING btree (jid);


CREATE TABLE rostergroups (
    username text NOT NULL,
    jid text NOT NULL,
    grp text NOT NULL
);

CREATE INDEX pk_rosterg_user_jid ON rostergroups USING btree (username, jid);

CREATE TABLE sr_group (
    name text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE sr_user (
    jid text NOT NULL,
    grp text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_sr_user_jid_grp ON sr_user USING btree (jid, grp);
CREATE INDEX i_sr_user_jid ON sr_user USING btree (jid);
CREATE INDEX i_sr_user_grp ON sr_user USING btree (grp);

CREATE TABLE spool (
    username text NOT NULL,
    xml text NOT NULL,
    seq SERIAL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_despool ON spool USING btree (username);

CREATE TABLE archive (
    username text NOT NULL,
    timestamp BIGINT NOT NULL,
    peer text NOT NULL,
    bare_peer text NOT NULL,
    xml text NOT NULL,
    txt text,
    id SERIAL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_username ON archive USING btree (username);
CREATE INDEX i_timestamp ON archive USING btree (timestamp);
CREATE INDEX i_peer ON archive USING btree (peer);
CREATE INDEX i_bare_peer ON archive USING btree (bare_peer);

CREATE TABLE archive_prefs (
    username text NOT NULL PRIMARY KEY,
    def text NOT NULL,
    always text NOT NULL,
    never text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE vcard (
    username text PRIMARY KEY,
    vcard text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE vcard_xupdate (
    username text PRIMARY KEY,
    hash text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE vcard_search (
    username text NOT NULL,
    lusername text PRIMARY KEY,
    fn text NOT NULL,
    lfn text NOT NULL,
    family text NOT NULL,
    lfamily text NOT NULL,
    given text NOT NULL,
    lgiven text NOT NULL,
    middle text NOT NULL,
    lmiddle text NOT NULL,
    nickname text NOT NULL,
    lnickname text NOT NULL,
    bday text NOT NULL,
    lbday text NOT NULL,
    ctry text NOT NULL,
    lctry text NOT NULL,
    locality text NOT NULL,
    llocality text NOT NULL,
    email text NOT NULL,
    lemail text NOT NULL,
    orgname text NOT NULL,
    lorgname text NOT NULL,
    orgunit text NOT NULL,
    lorgunit text NOT NULL
);

CREATE INDEX i_vcard_search_lfn       ON vcard_search(lfn);
CREATE INDEX i_vcard_search_lfamily   ON vcard_search(lfamily);
CREATE INDEX i_vcard_search_lgiven    ON vcard_search(lgiven);
CREATE INDEX i_vcard_search_lmiddle   ON vcard_search(lmiddle);
CREATE INDEX i_vcard_search_lnickname ON vcard_search(lnickname);
CREATE INDEX i_vcard_search_lbday     ON vcard_search(lbday);
CREATE INDEX i_vcard_search_lctry     ON vcard_search(lctry);
CREATE INDEX i_vcard_search_llocality ON vcard_search(llocality);
CREATE INDEX i_vcard_search_lemail    ON vcard_search(lemail);
CREATE INDEX i_vcard_search_lorgname  ON vcard_search(lorgname);
CREATE INDEX i_vcard_search_lorgunit  ON vcard_search(lorgunit);

CREATE TABLE privacy_default_list (
    username text PRIMARY KEY,
    name text NOT NULL
);

CREATE TABLE privacy_list (
    username text NOT NULL,
    name text NOT NULL,
    id SERIAL UNIQUE,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_privacy_list_username ON privacy_list USING btree (username);
CREATE UNIQUE INDEX i_privacy_list_username_name ON privacy_list USING btree (username, name);

CREATE TABLE privacy_list_data (
    id bigint REFERENCES privacy_list(id) ON DELETE CASCADE,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord NUMERIC NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
);

CREATE INDEX i_privacy_list_data_id ON privacy_list_data USING btree (id);

CREATE TABLE private_storage (
    username text NOT NULL,
    namespace text NOT NULL,
    data text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_private_storage_username ON private_storage USING btree (username);
CREATE UNIQUE INDEX i_private_storage_username_namespace ON private_storage USING btree (username, namespace);


CREATE TABLE roster_version (
    username text PRIMARY KEY,
    version text NOT NULL
);

-- To update from 0.9.8:
-- CREATE SEQUENCE spool_seq_seq;
-- ALTER TABLE spool ADD COLUMN seq integer;
-- ALTER TABLE spool ALTER COLUMN seq SET DEFAULT nextval('spool_seq_seq');
-- UPDATE spool SET seq = DEFAULT;
-- ALTER TABLE spool ALTER COLUMN seq SET NOT NULL;

-- To update from 1.x:
-- ALTER TABLE rosterusers ADD COLUMN askmessage text;
-- UPDATE rosterusers SET askmessage = '';
-- ALTER TABLE rosterusers ALTER COLUMN askmessage SET NOT NULL;

CREATE TABLE pubsub_node (
  host text,
  node text,
  parent text,
  "type" text,
  nodeid SERIAL UNIQUE
);
CREATE INDEX i_pubsub_node_parent ON pubsub_node USING btree (parent);
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node USING btree (host, node);

CREATE TABLE pubsub_node_option (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  name text,
  val text
);
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option USING btree (nodeid);

CREATE TABLE pubsub_node_owner (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  owner text
);
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner USING btree (nodeid);

CREATE TABLE pubsub_state (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  jid text,
  affiliation character(1),
  subscriptions text,
  stateid SERIAL UNIQUE
);
CREATE INDEX i_pubsub_state_jid ON pubsub_state USING btree (jid);
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state USING btree (nodeid, jid);

CREATE TABLE pubsub_item (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  itemid text, 
  publisher text,
  creation text,
  modification text,
  payload text
);
CREATE INDEX i_pubsub_item_itemid ON pubsub_item USING btree (itemid);
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item USING btree (nodeid, itemid);

CREATE TABLE pubsub_subscription_opt (
  subid text,
  opt_name varchar(32),
  opt_value text
);
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt USING btree (subid, opt_name);

CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_muc_room_name_host ON muc_room USING btree (name, host);

CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    nick text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_muc_registered_nick ON muc_registered USING btree (nick);
CREATE UNIQUE INDEX i_muc_registered_jid_host ON muc_registered USING btree (jid, host);

CREATE TABLE irc_custom (
    jid text NOT NULL,
    host text NOT NULL,
    data text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_irc_custom_jid_host ON irc_custom USING btree (jid, host);

CREATE TABLE motd (
    username text PRIMARY KEY,
    xml text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE caps_features (
    node text NOT NULL,
    subnode text NOT NULL,
    feature text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_caps_features_node_subnode ON caps_features USING btree (node, subnode);

CREATE TABLE sm (
    usec bigint NOT NULL,
    pid text NOT NULL,
    node text NOT NULL,
    username text NOT NULL,
    resource text NOT NULL,
    priority text NOT NULL,
    info text NOT NULL
);

CREATE UNIQUE INDEX i_sm_sid ON sm USING btree (usec, pid);
CREATE INDEX i_sm_node ON sm USING btree (node);
CREATE INDEX i_sm_username ON sm USING btree (username);
