-module(ls_network).

-export([
         create/1,
         create/2,
         delete/1,
         delete/2,
         get/1,
         add_iprange/2,
         add_iprange/3,
         remove_iprange/2,
         set_metadata/2,
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
                    {ok, UUID::fifo:network_id()} |
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
-spec delete(Network::fifo:network_id()) ->
                    ok | not_found |
                    {'error','no_servers'}.
delete(Network) when
      is_binary(Network) ->
    delete(mdns, Network).

delete(Sniffle, Network) when
      is_binary(Network) ->
    send(Sniffle, {network, delete, Network}).

%%--------------------------------------------------------------------
%% @doc Reads a network from the database
%% @end
%%--------------------------------------------------------------------
-spec get(Network::fifo:network_id()) ->
                 not_found |
                 {ok, Network::fifo:network()} |
                 {'error','no_servers'}.
get(Network) when
      is_binary(Network) ->
    send({network, get, Network}).

%%--------------------------------------------------------------------
%% @doc Adds a iprange to a network
%% @end
%%--------------------------------------------------------------------
-spec add_iprange(Network::fifo:network_id(), IPrange::fifo:iprange_id()) ->
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
-spec remove_iprange(Network::fifo:network_id(), IPrange::fifo:iprange_id()) ->
                            not_found |
                            ok |
                            {'error','no_servers'}.
remove_iprange(Network, IPRange) when
      is_binary(Network),
      is_binary(IPRange) ->
    send({network, remove_iprange, Network, IPRange}).

%%--------------------------------------------------------------------
%% @doc Lists all networks known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Reqs::[fifo:matcher()], boolean()) ->
                  {ok, [{Ranking::integer(), fifo:network_id()}]} |
                  {ok, [{Ranking::integer(), fifo:network()}]} |
                  {'error','no_servers'}.
list(Reqs, Full) ->
    list(mdns, Reqs, Full).

list(Sniffle, Reqs, Full) ->
    send(Sniffle, {network, list, Reqs, Full}).

-define(HS(F),
        F(DTRace, Val) ->
               send({network, F, DTRace, Val})).
?HS(uuid).

-spec name(fifo:network_id(), binary()) ->
                  ok |
                  {error, no_servers}.
?HS(name).
-spec set_metadata(fifo:network_id(), fifo:attr_list()) ->
                          ok |
                          {error, no_servers}.
?HS(set_metadata).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec send(MSG::fifo:sniffle_network_message()) ->
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
