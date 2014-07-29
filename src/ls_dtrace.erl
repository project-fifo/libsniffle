-module(ls_dtrace).

-export([
         add/2,
         delete/1,
         get/1,
         set/2,
         set/3,
         list/2,
         list/1,
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
                 Script::list()) ->
                        {ok, UUID::fifo:uuid()} |
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
                        {ok, Data::fifo:config_list()} |
                        not_found |
                        {'error','no_servers'}.
get(ID) when
      is_binary(ID)->
    send({dtrace, get, ID}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database.
%% @end
%%--------------------------------------------------------------------
-spec list() ->
                         {ok, [UUID::fifo:uuid()]} |
                         {'error','no_servers'}.
list()->
    send({dtrace, list}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec list([Requirement::fifo:matcher()]) ->
                         {ok, [{Ranking::integer(),
                                ID::fifo:id()}]} |
                         {'error','no_servers'}.
list(Requirements)->
    send({dtrace, list, Requirements}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec list([Requirement::fifo:matcher()], boolean()) ->
                         {ok, [{Ranking::integer(),
                                ID::fifo:id()|fifo:object()}]} |
                         {'error','no_servers'}.
list(Requirements, Full)->
    send({dtrace, list, Requirements, Full}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec set(Dtrace::fifo:uuid(),
                 Attribute::fifo:keys(),
                 Value::fifo:value() | delete) ->
                        ok | not_found |
                        {'error','no_servers'}.
set(DTrace, Attribute, Value) when
      is_binary(DTrace) ->
    send({dtrace, set, DTrace, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets options on a dtace script. The root key 'config' has a
%%   special meaning here since it holds replacement variables.
%% @end
%%--------------------------------------------------------------------
-spec set(DTrace::fifo:uuid(),
                 Attributes::fifo:config_list()) ->
                        ok | not_found |
                        {'error','no_servers'}.
set(DTrace, Attributes) when
      is_binary(DTrace) ->
    send({dtrace, set, DTrace, Attributes}).

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

?HS(name).
?HS(uuid).
?HS(script).
?HS(set_metadata).
?HS(set_config).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec send(MSG::fifo:sniffle_message()) ->
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
