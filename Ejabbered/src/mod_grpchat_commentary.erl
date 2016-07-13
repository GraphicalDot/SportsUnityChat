%%%----------------------------------------------------------------------
%%% File    : mod_grpchat_commentary.erl
%%% Author  : Aakarshi Goel <aakarshi92@gmaiil.com>
%%% Purpose :
%%% Usage   : Add the following line in modules section of ejabberd.cfg:
%%%              mod_grpchat_commentary: {}
%%%
%%%----------------------------------------------------------------------

-module(mod_grpchat_commentary).
-behaviour(gen_mod).

-include("logger.hrl").


-export([start/2, stop/1]).
-export([create_live_match_node/4]).



%% ==========================================
%% gen_mod functions
%% ==========================================

start(Host, _Opts) ->
  ejabberd_hooks:add(create_match_node, Host, ?MODULE, create_live_match_node, 10),
  ok.

stop(Host) ->
  ejabberd_hooks:delete(create_match_node, Host, ?MODULE, create_live_match_node, 10),
  ok.


%% ===========================================
%% Internal functions
%% ===========================================


%create_live_match_node(ServerHost, Host, From, LiveMatches) ->
%    Owner = mod_pubsub:service_jid(Host),
%    [NodeId, Rest] = binary:split(LiveMatches, <<",">>),
%    case mod_pubsub:create_node(Host, ServerHost, NodeId, Owner, <<"LiveMatch">>) of
%        {result, T} -> io:format(T),
%                        sleep(50);
%        Error -> io:format("Error is"),
%                 io:format(Error)
%    end,
%    create_live_match_node(ServerHost, Host, From, Rest).

subscribe_group_to_match(ServerHost, Host, From, SubscribedMatch, NodeId) ->
    ok.
%    mod_pubsub:subscribe_node(Host, NodeId, From, JID, Config);



create_live_match_node(ServerHost, Host, From, SubscribedMatch) ->
    Owner = mod_pubsub:service_jid(Host),
    [NodeId, _] = binary:split(SubscribedMatch, <<"-">>),
    case mod_pubsub:create_node(Host, ServerHost, NodeId, Owner, <<"SubscribedMatch">>) of
        {result, T} -> subscribe_group_to_match(ServerHost, Host, From, SubscribedMatch, NodeId),
                       sleep(50);
        {error, ErrorXML} -> case xml:get_subtag(ErrorXML, <<"conflict">>) of
                                NodeAlreadyPresent -> subscribe_group_to_match(ServerHost, Host, From, SubscribedMatch, NodeId);
                                false -> ErrorXML
                             end;
        _ -> ok
    end,
    ok.


sleep(N) ->
    timer:sleep(random:uniform(N)).

get_node_id(Match) ->
    [S,M|_] = binary:split(Match, <<",">>),
    [N,SeriesId|_] = binary:split(S, <<"'series_id': ">>),
    [T,MatchId|_] = binary:split(M, <<"'match_id': ">>),
    Node = <<SeriesId/binary, <<":">>/binary, MatchId/binary>>,
    {Node}.


