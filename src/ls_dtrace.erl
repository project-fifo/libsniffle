-module(ls_dtrace).

-export([
         add/2,
         delete/1,
         get/1,
         list/2,
         list/0,
         run/2
        ]).

-export([
         name/2,
         uuid/2,
         script/2,
         set_metadata/2,
         set_config/2
        ]).


%%%===================================================================
%%% DTrace Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds a new dtrace script to sniffle, the name is a plain
%%   binary while the script is a string that can contain placeholders
%%   encapsulated in $ signs.
%%   A UUID is returned.
%% @end
%%--------------------------------------------------------------------
-spec add(Name::binary(),
          Script::string()) ->
                 {ok, UUID::fifo:dtrace_id()} |
                 duplicate |
                 {'error','no_servers'}.
add(Name, Script) when
      is_binary(Name),
      is_list(Script)->
    send({dtrace, add, Name, Script}).

%%--------------------------------------------------------------------
%% @doc Deletes a dtrace script from the library
%% @end
%%--------------------------------------------------------------------
-spec delete(UUID::fifo:uuid()) ->
                    ok |
                    {'error','no_servers'}.
delete(ID) when
      is_binary(ID)->
    send({dtrace, delete, ID}).

%%--------------------------------------------------------------------
%% @doc Reads a dtrace script and returns the jsx object for it.
%% @end
%%--------------------------------------------------------------------
-spec get(UUID::fifo:uuid()) ->
                 not_found |
                 {ok, Data::fifo:dataset()} |
                 {'error','no_servers'}.
get(ID) when
      is_binary(ID)->
    send({dtrace, get, ID}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database.
%% @end
%%--------------------------------------------------------------------
-spec list() ->
                  {ok, [UUID::fifo:dataset_id()]} |
                  {'error','no_servers'}.
list()->
    send({dtrace, list}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec list([Requirement::fifo:matcher()], boolean()) ->
                  {ok, [{Ranking::integer(), ID::fifo:dataset_id()}]} |
                  {ok, [{Ranking::integer(), Dataset::fifo:dataset()}]} |
                  {'error','no_servers'}.
list(Requirements, Full)->
    send({dtrace, list, Requirements, Full}).


%%--------------------------------------------------------------------
%% @doc Starts a dtrace script on the given hypervisors, returns an
%%   opened tcp socket that will stream incoming data every second and
%%   allows some interaction.
%% @end
%%--------------------------------------------------------------------
-spec run(DTrace::fifo:uuid(),
          Servers::[fifo:hypervisor()]) ->
                 {ok, Socket::port()} |
                 not_found |
                 {'error','no_servers'}.

run(ID, Servers) when
      is_binary(ID)->
    case libsniffle_server:get_server() of
        {error, no_server} ->
            {error, no_server};
        {ok, Server, Port} ->
            case gen_tcp:connect(Server, Port, [binary, {active, true}, {packet, 4}], 100) of
                {ok, Socket} ->
                    ok = gen_tcp:send(Socket, term_to_binary({dtrace, run, ID, Servers})),
                    {ok, Socket};
                E ->
                    E
            end
    end.

-define(HS(F),
        F(DTRace, Val) ->
               send({dtrace, F, DTRace, Val})).

-spec uuid(ID::fifo:dtrace_id(), binary()) ->
                  ok | {'error','no_servers'}.
?HS(uuid).

-spec name(ID::fifo:dtrace_id(), binary()) ->
                  ok | {'error','no_servers'}.
?HS(name).


-spec script(ID::fifo:dtrace_id(), string()) ->
                    ok | {'error','no_servers'}.
?HS(script).

-spec set_metadata(ID::fifo:dtrace_id(), [{jsxd:key(), jsxd:value()}]) ->
                          ok | {'error','no_servers'}.
?HS(set_metadata).

-spec set_config(ID::fifo:dtrace_id(), [{jsxd:key(), jsxd:value()}]) ->
                        ok | {'error','no_servers'}.
?HS(set_config).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec send(MSG::fifo:sniffle_dtrace_message()) ->
                  ok |
                  atom() |
                  {ok, Reply::term()} |
                  {error, no_servers}.
send(Msg) ->
    send(mdns, Msg).
send(Sniffle, Msg) ->
    case libsniffle_server:send(Sniffle, Msg) of
        {reply, Reply} ->
            Reply;
        noreply ->
            ok;
        E ->
            E
    end.
