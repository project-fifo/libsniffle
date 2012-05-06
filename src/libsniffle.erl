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
	 list_keys/1,
	 ping/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list_machines(Auth) ->
    Sniffle = gproc:lookup_pid({n, g, sniffle}),
    gen_server:call(Sniffle, {machines, list, Auth}).


list_keys(Auth) ->
    Sniffle = gproc:lookup_pid({n, g, sniffle}),
    gen_server:call(Sniffle, {keys, list, Auth}).

ping() ->
    Sniffle = gproc:lookup_pid({n, g, sniffle}),
    gen_server:call(Sniffle, ping).



%%%===================================================================
%%% Internal functions
%%%===================================================================
