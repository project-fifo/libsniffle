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
	 start_machine/2,
	 stop_machine/2,
	 reboot_machine/2,
	 list_packages/1,
	 list_datasets/1,
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

start_machine(Auth, UUID) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {machines, start, Auth, UUID}).

stop_machine(Auth, UUID) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {machines, stop, Auth, UUID}).

reboot_machine(Auth, UUID) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {machines, reboot, Auth, UUID}).

create_machine(Auth, Data) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {machines, create, Data}).

list_datasets(Auth) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {datasets, list, Auth}).

list_packages(Auth) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {packages, list, Auth}).

list_keys(Auth) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {keys, list, Auth}).

create_key(Auth, Pass, KeyID, PublicKey) ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, {keys, create, Auth, Pass, KeyID, PublicKey}).

ping() ->
    Sniffle = sniffle(),
    gen_server:call(Sniffle, ping).




%%%===================================================================
%%% Internal functions
%%%===================================================================

sniffle() ->
    gproc:lookup_pid({n, g, sniffle}).
