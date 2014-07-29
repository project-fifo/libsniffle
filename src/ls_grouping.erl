-module(ls_grouping).

-export([
         add/2,
         delete/1,
         get/1,
         metadata_set/2,
         metadata_set/3,
         list/2,
         list/1,
         list/0,
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
                          {ok, UUID::fifo:uuid()} |
                          duplicate |
                          {'error','no_servers'}.
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
-spec delete(UUID::fifo:uuid()) ->
                           ok |
                           {'error','no_servers'}.
delete(ID) when
      is_binary(ID)->
    send({grouping, delete, ID}).

%%--------------------------------------------------------------------
%% @doc Reads a grouping script and returns the jsx object for it.
%% @end
%%--------------------------------------------------------------------
-spec get(UUID::fifo:uuid()) ->
                        {ok, Data::fifo:config_list()} |
                        not_found |
                        {'error','no_servers'}.
get(ID) when
      is_binary(ID)->
    send({grouping, get, ID}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database.
%% @end
%%--------------------------------------------------------------------
-spec list() ->
                         {ok, [UUID::fifo:uuid()]} |
                         {'error','no_servers'}.
list()->
    send({grouping, list}).

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
    send({grouping, list, Requirements}).

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
    send({grouping, list, Requirements, Full}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec metadata_set(Grouping::fifo:uuid(),
                 Attribute::fifo:keys(),
                 Value::fifo:value() | delete) ->
                        ok | not_found |
                        {'error','no_servers'}.
metadata_set(Grouping, Attribute, Value) when
      is_binary(Grouping) ->
    send({grouping, metadata, set, Grouping, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets options on a dtace script. The root key 'config' has a
%%   special meaning here since it holds replacement variables.
%% @end
%%--------------------------------------------------------------------
-spec metadata_set(Grouping::fifo:uuid(),
                 Attributes::fifo:config_list()) ->
                        ok | not_found |
                        {'error','no_servers'}.
metadata_set(Grouping, Attributes) when
      is_binary(Grouping) ->
    send({grouping, metadata, set, Grouping, Attributes}).


add_element(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, element, add, Grouping, Element}).

remove_element(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, element, remove, Grouping, Element}).

add_grouping(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, grouping, add, Grouping, Element}).

remove_grouping(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, grouping, remove, Grouping, Element}).

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
