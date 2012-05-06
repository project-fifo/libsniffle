%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  5 May 2012 by Heinz N. Gies <>
%%%-------------------------------------------------------------------
-module(libsniffle).

%% API
-export([list_machines/1,
	 get_machine/2,
	 delete_machine/2,
	 list_keys/1,
	 ping/0,
	 create_key/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list_machines(Auth) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {machines, list, Auth}).

get_machine(Auth, UUID) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {machines, get, Auth, UUID}).

delete_machine(Auth, UUID) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {machines, delete, Auth, UUID}).
    
list_keys(Auth) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {keys, list, Auth}).


ping() ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, ping).

create_key(Auth, Pass, KeyID, PublicKey) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {keys, create, Auth, Pass, KeyID, PublicKey}).



%%%===================================================================
%%% Internal functions
%%%===================================================================

sniffle() ->
    gproc:lookup_pid({n, g, sniffle}).
