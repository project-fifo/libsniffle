-module(ls_grouping).

-export([
         add/2,
         delete/1,
         get/1,
         set_metadata/2,
         set_config/2,
         list/2,
         list/0,
         stream/3,
         add_element/2,
         remove_element/2,
         add_grouping/2,
         remove_grouping/2
        ]).

%%%===================================================================
%%% Grouping Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds a new grouping to sniffle, the name is a plain
%%   binary, the type is the atom cluster or stack
%%   encapsulated in $ signs.
%%   A UUID is returned.
%% @end
%%--------------------------------------------------------------------
-spec add(Name::binary(),
          Type::atom()) ->
                 {ok, UUID::fifo:grouping_id()} |
                 duplicate |
                 {'error', 'no_servers'}.
add(Name, cluster) when
      is_binary(Name)->
    send({grouping, add, Name, cluster});
add(Name, none) when
      is_binary(Name)->
    send({grouping, add, Name, none});
add(Name, stack) when
      is_binary(Name)->
    send({grouping, add, Name, stack}).

%%--------------------------------------------------------------------
%% @doc Deletes a grouping script from the library
%% @end
%%--------------------------------------------------------------------
-spec delete(UUID::fifo:grouping_id()) ->
                    ok |
                    {'error', 'no_servers'}.
delete(ID) when
      is_binary(ID)->
    send({grouping, delete, ID}).

%%--------------------------------------------------------------------
%% @doc Reads a grouping script and returns the jsx object for it.
%% @end
%%--------------------------------------------------------------------
-spec get(UUID::fifo:grouping_id()) ->
                 {ok, Data::fifo:grouping()} |
                 not_found |
                 {'error', 'no_servers'}.
get(ID) when
      is_binary(ID)->
    send({grouping, get, ID}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database.
%% @end
%%--------------------------------------------------------------------
-spec list() ->
                  {ok, [UUID::fifo:grouping_id()]} |
                  {'error', 'no_servers'}.
list()->
    send({grouping, list}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec list([Requirement::fifo:matcher()], boolean()) ->
                  {ok, [{Ranking::integer(), ID::fifo:grouping_id()}]} |
                  {ok, [{Ranking::integer(), ID::fifo:grouping()}]} |
                  {'error', 'no_servers'}.
list(Requirements, Full)->
    send({grouping, list, Requirements, Full}).

%%--------------------------------------------------------------------
%% @doc Sets the metadat for a grouping.
%% @end
%%--------------------------------------------------------------------
-spec set_metadata(Grouping::fifo:grouping_id(),
                   Attributes::fifo:attr_list()) ->
                          ok | not_found |
                          {'error', 'no_servers'}.
set_metadata(Grouping, Attributes) when
      is_binary(Grouping) ->
    send({grouping, metadata, set, Grouping, Attributes}).


%%--------------------------------------------------------------------
%% @doc Sets the cluster/stack config for a grouping.
%% @end
%%--------------------------------------------------------------------
-spec set_config(Grouping::fifo:grouping_id(),
                 Attributes::fifo:attr_list()) ->
                        ok | not_found |
                        {'error', 'no_servers'}.
set_config(Grouping, Attributes) when
      is_binary(Grouping) ->
    send({grouping, config, set, Grouping, Attributes}).

%%--------------------------------------------------------------------
%% @doc Adds or removes a element (child) to a grouping, children can
%%   be either a grouping or a VM.
%%--------------------------------------------------------------------
-spec add_element(fifo:grouping_id(), fifo:vm_id() | fifo:grouping_id()) ->
                         ok | not_found | {error, no_servers}.
add_element(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, element, add, Grouping, Element}).

-spec remove_element(fifo:grouping_id(), fifo:vm_id()) ->
                            ok | not_found | {error, r_servers}.
remove_element(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, element, remove, Grouping, Element}).

%%--------------------------------------------------------------------
%% @doc Adds or removes a grouping (paremnt) to a grouping, parents
%%   can only be groupings.
%%--------------------------------------------------------------------
-spec add_grouping(fifo:grouping_id(), fifo:vm_id()) ->
                          ok | not_found | {error, r_servers}.
add_grouping(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, grouping, add, Grouping, Element}).

-spec remove_grouping(fifo:grouping_id(), fifo:grouping_id()) ->
                             ok | not_found | {error, r_servers}.
remove_grouping(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, grouping, remove, Grouping, Element}).

%%--------------------------------------------------------------------
%% @doc Streams the Grouping scripts in chunks.
%% @end
%%--------------------------------------------------------------------
-spec stream(Reqs::[fifo:matcher()], mdns_client_lib:stream_fun(), term()) ->
                  {ok, [{Ranking::integer(), fifo:grouping_id()}]} |
                  {ok, [{Ranking::integer(), fifo:grouping()}]} |
                  {'error', 'no_servers'}.
stream(Reqs, StreamFn, Acc0) ->
    case libsniffle_server:stream({grouping, stream, Reqs}, StreamFn, Acc0) of
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

-spec send(MSG::fifo:sniffle_grouping_message()) ->
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
