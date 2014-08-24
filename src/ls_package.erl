-module(ls_package).

-export([
         create/1,
         delete/1,
         get/1,
         list/0,
         list/2
        ]).


-export([
         set_metadata/2,
         blocksize/2,
         compression/2,
         cpu_cap/2,
         cpu_shares/2,
         max_swap/2,
         name/2,
         quota/2,
         ram/2,
         uuid/2,
         zfs_io_priority/2,
         remove_requirement/2,
         add_requirement/2
        ]).

-define(UUID, <<UUID:36/binary>>).

%%%===================================================================
%%%  PACKAGE Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new package and returns it's UUID.
%% @end
%%--------------------------------------------------------------------
-spec create(Name::binary()) ->
                            {ok, UUID::fifo:package_id()} |
                            duplicate |
                            {'error','no_servers'}.
create(Name) when
      is_binary(Name) ->
    send({package, create, Name}).

%%--------------------------------------------------------------------
%% @doc Deletes a package from the database
%% @end
%%--------------------------------------------------------------------
-spec delete(Package::fifo:uuid()) ->
                            ok | not_found |
                            {'error','no_servers'}.
delete(Package) when
      is_binary(Package) ->
    send({package, delete, Package}).

%%--------------------------------------------------------------------
%% @doc Reads a package from the database
%% @end
%%--------------------------------------------------------------------
-spec get(Package::binary()) ->
                         not_found |
                         {ok, Package::fifo:package()} |
                         {'error','no_servers'}.
get(Package) when
      is_binary(Package) ->
    send({package, get, Package}).

%%--------------------------------------------------------------------
%% @doc Lists all packages known to the system.
%% @end
%%--------------------------------------------------------------------
-spec list() ->
                  {ok, Packages::[fifo:package_id()]} |
                  {'error','no_servers'}.
list() ->
    send({package, list}).

%%--------------------------------------------------------------------
%% @doc Lists all packages known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Reqs::[fifo:matcher()], boolean()) ->
                  {ok, [{Ranking::integer(), ID::fifo:package_id()}]} |
                  {ok, [{Ranking::integer(), ID::fifo:package()}]} |
                  {'error','no_servers'}.
list(Reqs, Full) ->
    send({package, list, Reqs, Full}).

-define(HS(F),
        F(DTRace, Val) ->
               send({package, F, DTRace, Val})).

?HS(set_metadata).
?HS(blocksize).
?HS(compression).
?HS(cpu_cap).
?HS(cpu_shares).
?HS(max_swap).
?HS(name).
?HS(quota).
?HS(ram).
?HS(uuid).
?HS(zfs_io_priority).
?HS(remove_requirement).
?HS(add_requirement).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec send(MSG::fifo:sniffle_package_message()) ->
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
