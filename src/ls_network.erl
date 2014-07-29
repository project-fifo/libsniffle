-module(ls_network).

-export([
         create/1,
         create/2,
         delete/1,
         get/1,
         add_iprange/2,
         add_iprange/3,
         remove_iprange/2,
         set/2,
         set_metadata/2,
         set/3,
         list/2,
         list/3,
         name/2,
         uuid/2
        ]).

%%%===================================================================
%%%  NETWORK Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new network and returns it's UUID.
%% @end
%%--------------------------------------------------------------------
-spec create(Name::binary()) ->
                            {ok, UUID::fifo:uuid()} |
                            duplicate |
                            {'error','no_servers'}.
create(Name) ->
    create(mdns, Name).

create(Sniffle, Name) when
      is_binary(Name) ->
    send(Sniffle, {network, create, Name}).

%%--------------------------------------------------------------------
%% @doc Deletes a network from the database
%% @end
%%--------------------------------------------------------------------
-spec delete(Network::fifo:uuid()) ->
                            ok | not_found |
                            {'error','no_servers'}.
delete(Network) when
      is_binary(Network) ->
    send({network, delete, Network}).

%%--------------------------------------------------------------------
%% @doc Reads a network from the database
%% @end
%%--------------------------------------------------------------------
-spec get(Network::binary()) ->
                         not_found |
                         {ok, Network::fifo:config_list()} |
                         {'error','no_servers'}.
get(Network) when
      is_binary(Network) ->
    send({network, get, Network}).

%%--------------------------------------------------------------------
%% @doc Adds a iprange to a network
%% @end
%%--------------------------------------------------------------------
-spec add_iprange(Network::binary(), IPrange::binary()) ->
                                 not_found |
                                 ok |
                                 {'error','no_servers'}.
add_iprange(Network, IPRange) ->
      add_iprange(mdns, Network, IPRange).

add_iprange(Sniffle, Network, IPRange) when
      is_binary(Network),
      is_binary(IPRange) ->
    send(Sniffle, {network, add_iprange, Network, IPRange}).

%%--------------------------------------------------------------------
%% @doc Adds a iprange to a network
%% @end
%%--------------------------------------------------------------------
-spec remove_iprange(Network::binary(), IPrange::binary()) ->
                                    not_found |
                                    ok |
                                    {'error','no_servers'}.
remove_iprange(Network, IPRange) when
      is_binary(Network),
      is_binary(IPRange) ->
    send({network, remove_iprange, Network, IPRange}).

%%--------------------------------------------------------------------
%% @doc Sets a attribute on the pacakge.
%% @end
%%--------------------------------------------------------------------
-spec set(Network::fifo:id(),
                  Attribute::fifo:keys(),
                  Value::fifo:value() | delete) -> ok | not_found |
                                                   {'error','no_servers'}.
set(Network, Attribute, Value)  when
      is_binary(Network) ->
    send({network, set, Network, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets multiple attributes on the pacakge.
%% @end
%%--------------------------------------------------------------------
-spec set(Network::fifo:id(),
                  Attirbutes::fifo:config_list()) ->
                         ok | not_found |
                         {'error','no_servers'}.
set(Network, Attributes) when
      is_binary(Network),
      is_list(Attributes) ->
    send({network, set, Network, Attributes}).

%%--------------------------------------------------------------------
%% @doc Lists all networks known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Reqs::[fifo:matcher()], boolean()) ->
                          {ok, [{Ranking::integer(),
                                 ID::fifo:id()|fifo:object()}]} |
                          {'error','no_servers'}.
list(Reqs, Full) ->
    list(mdns, Reqs, Full).

list(Sniffle, Reqs, Full) ->
    send(Sniffle, {network, list, Reqs, Full}).

-define(HS(F),
        F(DTRace, Val) ->
               send({network, F, DTRace, Val})).
?HS(uuid).
?HS(name).
?HS(set_metadata).

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
