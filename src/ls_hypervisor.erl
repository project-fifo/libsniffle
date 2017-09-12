-module(ls_hypervisor).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         register/3,
         unregister/1,
         service_action/3,
         get/1,
         list/0,
         list/2,
         stream/3,
         set_resource/2,
         set_characteristic/2,
         set_metadata/2,
         set_pool/2,
         set_service/2,
         alias/2,
         architecture/2,
         etherstubs/2,
         host/2,
         networks/2,
         path/2,
         port/2,
         sysinfo/2,
         uuid/2,
         version/2,
         last_seen/2,
         virtualisation/2
        ]).

%%%===================================================================
%%% Hypervisor Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Registeres a hypervisor with sniffle, passing it's ID
%%   (usually the hostname) the ip and the Port of the chunter server
%%   running on it.
%% @end
%%--------------------------------------------------------------------
-spec register(Hypervisor::binary(),
               Host::binary(),
               Port::inet:port_number()) ->
                      ok |
                      {'error', 'no_servers'}.
register(Hypervisor, Host, Port) when
      is_binary(Hypervisor),
      is_integer(Port),
      Port > 0 ->
    send({hypervisor, register, Hypervisor, Host, Port}).

%%--------------------------------------------------------------------
%% @doc Removes a hypervisor form sniffle.
%% @end
%%--------------------------------------------------------------------
-spec unregister(Hypervisor::binary()) ->
                        ok | not_found |
                        {'error', 'no_servers'}.
unregister(Hypervisor) ->
    send({hypervisor, unregister, Hypervisor}).

%%--------------------------------------------------------------------
%% @doc Enables a service on a hypervisor.
%% @end
%%--------------------------------------------------------------------

-spec service_action(
        Hypervisor::fifo:uuid(),
        Action::enable|disable|clear|refresh|restart,
        Service::binary()) ->
                            ok | not_found |
                            {'error', 'no_servers'}.
service_action(Hypervisor, Action, Service) when
      Action =:= enable;
      Action =:= refresh;
      Action =:= restart;
      Action =:= disable;
      Action =:= clear ->
    send({hypervisor, service, Hypervisor, Action, Service}).


%%--------------------------------------------------------------------
%% @doc Reads the hypervisor object from the database.
%% @end
%%--------------------------------------------------------------------
-spec get(Hypervisor::binary()) ->
                 not_found |
                 {ok, fifo:hypervisor()} |
                 {'error', 'no_servers'}.
get(Hypervisor) ->
    send({hypervisor, get, Hypervisor}).

%%--------------------------------------------------------------------
%% @doc Lists all hypervisors known to the system.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [binary()]} |
                {'error', 'no_servers'}.
list() ->
    send({hypervisor, list}).

%%--------------------------------------------------------------------
%% @doc Lists all hypervisors known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Requirements::[fifo:matcher()], boolean()) ->
                  {ok, [{Ranking::integer(), ID::fifo:hypervisor_id()}]} |
                  {ok, [{Ranking::integer(), ID::fifo:hypervisor()}]} |
                  {'error', 'no_servers'}.
list(Requirements, Full) ->
    send({hypervisor, list, Requirements, Full}).

-define(HS(F),
        F(Hypervisor, Val) ->
               send({hypervisor, F, Hypervisor, Val})).

?HS(set_resource).
?HS(set_characteristic).
?HS(set_metadata).
?HS(set_pool).
?HS(set_service).
?HS(alias).
?HS(architecture).
?HS(etherstubs).
?HS(host).
?HS(networks).
?HS(path).
?HS(port).
?HS(sysinfo).
?HS(uuid).
?HS(version).
?HS(last_seen).
?HS(virtualisation).


%%--------------------------------------------------------------------
%% @doc Streams the HYPERVISOR's in chunks.
%% @end
%%--------------------------------------------------------------------
-spec stream(Reqs::[fifo:matcher()], mdns_client_lib:stream_fun(), term()) ->
                  {ok, [{Ranking::integer(), fifo:hypervisor_id()}]} |
                  {ok, [{Ranking::integer(), fifo:hypervisor()}]} |
                  {'error', 'no_servers'}.
stream(Reqs, StreamFn, Acc0) ->
    case libsniffle_server:stream({hypervisor, stream, Reqs}, StreamFn, Acc0) of
        {reply, Reply} ->
            Reply;
        noreply ->
            ok;
        E ->
            E
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec send(MSG::fifo:sniffle_hypervisor_message()) ->
                  ok |
                  atom() |
                  {ok, Reply::term()} |
                  {error, no_servers}.
send(Msg) ->
    send(mdns, Msg).
send(Sniffle, Msg) ->
    case libsniffle_server:sync(Sniffle, Msg) of
        {reply, Reply} ->
            Reply;
        noreply ->
            ok;
        E ->
            E
    end.
