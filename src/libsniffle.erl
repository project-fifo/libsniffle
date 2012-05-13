%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  5 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(libsniffle).

%% API
-export([list_machines/1,
	 get_machine/2,
	 get_machine_info/2,
	 create_machine/2,
	 delete_machine/2,
	 start_machine/2,
	 start_machine/3,
	 stop_machine/2,
	 reboot_machine/2,
	 list_packages/1,
	 list_datasets/1,
	 list_keys/1,
	 list_images/1,
	 register/3,
	 info/1,
	 ping/1,
	 create_key/5]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list_machines(Auth) ->
    sniffle_call(Auth, {machines, list}).

get_machine(Auth, UUID) ->
    sniffle_call(Auth, {machines, get, UUID}).

get_machine_info(Auth, UUID) ->
    sniffle_call(Auth, {machines, info, UUID}).

delete_machine(Auth, UUID) ->
    sniffle_call(Auth, {machines, delete, UUID}).

start_machine(Auth, UUID) ->
    sniffle_call(Auth, {machines, start, UUID}).

start_machine(Auth, UUID, Image) ->
    sniffle_call(Auth, {machines, start, UUID, [Image]}).

stop_machine(Auth, UUID) ->
    sniffle_call(Auth, {machines, stop, UUID}).

reboot_machine(Auth, UUID) ->
    sniffle_call(Auth, {machines, reboot, UUID}).

create_machine(Auth, Data) ->
    sniffle_call(Auth, {machines, create, Data}).

list_datasets(Auth) ->
    sniffle_call(Auth, {datasets, list}).

list_packages(Auth) ->
    sniffle_call(Auth, {packages, list}).

list_images(Auth) ->
    sniffle_call(Auth, {images, list}).

list_keys(Auth) ->
    sniffle_call(Auth, {keys, list}).

create_key(Auth, Name, Pass, KeyID, PublicKey) ->
    sniffle_call(Auth, {keys, create, Name, Pass, KeyID, PublicKey}).

info(Auth) ->
    sniffle_call(Auth, info).

ping(Auth) ->
    sniffle_call(Auth, ping).

register(Auth, Type, Spec) ->
    sniffle_cast(Auth, {register, Type, Spec}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

sniffle_call({Auth, _}, Call) ->
    sniffle_call(Auth, Call);

sniffle_call(Auth, Call) ->
    gen_server:call(sniffle(), {call, Auth, Call}).

sniffle_cast({Auth, _}, Cast) ->
    sniffle_cast(Auth, Cast);

sniffle_cast(Auth, Cast) ->
    gen_server:cast(sniffle(), {cast, Auth, Cast}).

    
sniffle() ->
    gproc:lookup_pid({n, g, sniffle}).
