import os
import sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from common import funcs
from sleekxmpp import ClientXMPP
from sleekxmpp.stanza import Message
import json
import requests
import schedule
import time


class PubSubMessageClient(ClientXMPP):

    def __init__(self, client_jid, client_password, new_live_matches):
        self.new_live_matches = new_live_matches
        self.client_jid = client_jid
        self.client_password = client_password
        ClientXMPP.__init__(self, self.client_jid, self.client_password)
        self._start_thread("chat_send", self.chat_send)

    def chat_send(self):
        print 'inside chat send'
        self.send_message(mto='pubsub.mm.io', mbody=', '.join(self.new_live_matches), msubject="New Live matches", mtype='chat',
                          mfrom=self.client_jid)




def grpchat_comm_fun(new_matches):
    print 'INSIDE grpchat_comm_fun'
    map(lambda match: funcs.QueryHandler.execute("INSERT INTO live_matches (match_id, series_id) VALUES (%s, %s);",
                                                 (match.split(':')[1], match.split(':')[0])), new_matches)

    xmpp = PubSubMessageClient(client_jid='aakarshi@mm.io', client_password='password', new_live_matches=new_matches)
    print "xmpp::", xmpp
    if xmpp.connect(('localhost', 5222)):
        xmpp.process(block=True)
        print("Done")
    else:
        print("Unable to connect.")



def get_live_matches():
    print "Inside get live matches"
    GET_ALL_MATCH_LIST_URL = 'http://scoreslb-822670678.ap-northeast-2.elb.amazonaws.com/v1/get_all_matches_list'
    response = requests.get(GET_ALL_MATCH_LIST_URL)
    res = json.loads(response.text)

    live_matches = [str(match['series_id']) + ':' + str(match['match_id'])
                       for match in res['data']['cricket'] if match['status'] == 'F']   # to be corrected: for upcoming and live both
    live_matches.extend([str(match['series_match_id']) + ':' + str(match['match_id'])
                        for match in res['data']['football'] if match['live'] == False])  # to be corrected:for upcoming and live both
    print 'live matches::::', len(live_matches)

    # get all matches whose node is already created
    already_saved = funcs.QueryHandler.get_results("SELECT match_id, series_id FROM live_matches;")
    print 'already saved::::', len(already_saved)

    new_matches = [match for match in live_matches if match not in already_saved]
    print 'new_live_matches:::', new_matches

    # INSERT INTO DB
    try:
        grpchat_comm_fun(new_matches)
    except Exception, e:
        print "ERROR IN INSERTION: %s" % e
    finally:
        print 'INSIDE FINALLY'




schedule.every(30).seconds.do(get_live_matches)

while True:
    schedule.run_pending()
    time.sleep(1)
