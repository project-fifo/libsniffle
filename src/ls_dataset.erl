-module(ls_dataset).

-export([
         create/1,
         import/1,
         delete/1,
         get/1,
         list/0,
         list/2,
         stream/3,
         available/0,
         available/2
        ]).

-export([
         set_metadata/2,
         imported/2,
         status/2,
         sha1/2,
         description/2,
         disk_driver/2,
         homepage/2,
         image_size/2,
         name/2,
         add_network/2,
         remove_network/2,
         add_requirement/2,
         remove_requirement/2,
         nic_driver/2,
         os/2,
         type/2,
         zone_type/2,
         users/2,
         version/2,
         kernel_version/2
        ]).

%%%===================================================================
%%%  DATASET Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Stream available datasets
%% @end
%%--------------------------------------------------------------------
-spec available(mdns_client_lib:stream_fun(), term()) ->
                       {ok, [maps:map()]} |
                       ok |
                       {'error', 'no_servers'}.
available(StreamFn, Acc0) ->
    case libsniffle_server:stream({dataset, available, stream},
                                  StreamFn, Acc0) of
        {reply, Reply} ->
            Reply;
        noreply ->
            ok;
        E ->
            E
    end.

%%--------------------------------------------------------------------
%% @doc Available datasets
%% @end
%%--------------------------------------------------------------------
-spec available() ->
                       {ok, [maps:map()]} |
                       {'error', 'no_servers'}.
available() ->
    send({dataset, available}).

%%--------------------------------------------------------------------
%% @doc Creates a new dataset with the given UUID.
%% @end
%%--------------------------------------------------------------------
-spec create(Dataset::binary()) ->
                    ok | duplicate |
                    {'error', 'no_servers'}.
create(Dataset) ->
    send({dataset, create, Dataset}).

%%--------------------------------------------------------------------
%% @doc Tries to import a dataset from a dsapi server URL.
%% @end
%%--------------------------------------------------------------------
-spec import(URL::binary()) ->
                    {ok, UUID::fifo:dataset_id()} |
                    {error, Reason::term()} |
                    {'error', 'no_servers'}.
import(URL) ->
    send({dataset, import, URL}).

%%--------------------------------------------------------------------
%% @doc Deletes a dataset from the server, this will also delete the
%%   image from the database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Dataset::binary()) ->
                    ok | not_found |
                    {'error', 'no_servers'}.
delete(Dataset) ->
    send({dataset, delete, Dataset}).

%%--------------------------------------------------------------------
%% @doc Gets a dataset from the database.
%% @end
%%--------------------------------------------------------------------
-spec get(Dataset::binary()) ->
                 {'error', 'no_servers'} |
                 not_found |
                 {ok, fifo:dataset()}.
get(Dataset) ->
    send({dataset, get, Dataset}).

%%--------------------------------------------------------------------
%% @doc Lists all datasets known to the system.
%% @end
%%--------------------------------------------------------------------
-spec list() ->
                  {ok, Datasets::[fifo:dataset_id()]} |
                  {'error', 'no_servers'}.
list() ->
    send({dataset, list}).

%%--------------------------------------------------------------------
%% @doc Lists all datasets known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Reqs::term(), boolean()) ->
                  {ok, Datasets::[{Ranking::integer(),
                                   ID::fifo:dataset_id()}]} |
                  {ok, Datasets::[{Ranking::integer(), Dset::fifo:dataset()}]} |
                  {'error', 'no_servers'}.
list(Reqs, Full) ->
    send({dataset, list, Reqs, Full}).


%%--------------------------------------------------------------------
%% @doc Streams the Datasets in chunks.
%% @end
%%--------------------------------------------------------------------
-spec stream(Reqs::[fifo:matcher()], mdns_client_lib:stream_fun(), term()) ->
                  {ok, [{Ranking::integer(), fifo:vm_id()}]} |
                  {ok, [{Ranking::integer(), fifo:vm()}]} |
                  {'error', 'no_servers'}.
stream(Reqs, StreamFn, Acc0) ->
    case libsniffle_server:stream({dataset, stream, Reqs}, StreamFn, Acc0) of
        {reply, Reply} ->
            Reply;
        noreply ->
            ok;
        E ->
            E
    end.

-define(HS(F),
        F(Dataset, Val) ->
               send({dataset, F, Dataset, Val})).

-spec status(fifo:dataset_id(), binary()) ->
                    ok | {error, no_servers}.
?HS(status).

-spec sha1(fifo:dataset_id(), binary()) ->
                  ok | {error, no_servers}.
?HS(sha1).

-spec imported(fifo:dataset_id(), float() | non_neg_integer()) ->
                      ok | {error, no_servers}.
?HS(imported).

-spec description(fifo:dataset_id(), binary()) ->
                         ok | {error, no_servers}.
?HS(description).

-spec disk_driver(fifo:dataset_id(), binary()) ->
                         ok | {error, no_servers}.
?HS(disk_driver).

-spec homepage(fifo:dataset_id(), binary()) ->
                      ok | {error, no_servers}.
?HS(homepage).

-spec image_size(fifo:dataset_id(), pos_integer()) ->
                        ok | {error, no_servers}.
?HS(image_size).

-spec name(fifo:dataset_id(), binary()) ->
                  ok | {error, no_servers}.
?HS(name).

-spec add_network(fifo:dataset_id(), {binary(), binary()}) ->
                         ok | {error, no_servers}.
?HS(add_network).

-spec remove_network(fifo:dataset_id(), {binary(), binary()}) ->
                            ok | {error, no_servers}.
?HS(remove_network).

-spec add_requirement(fifo:dataset_id(), fifo:matcher()) ->
                             ok | {error, no_servers}.
?HS(add_requirement).

-spec remove_requirement(fifo:dataset_id(), {binary(), binary()}) ->
                                ok | {error, no_servers}.
?HS(remove_requirement).

-spec nic_driver(fifo:dataset_id(), binary()) ->
                        ok | {error, no_servers}.
?HS(nic_driver).

-spec os(fifo:dataset_id(), binary()) ->
                ok | {error, no_servers}.
?HS(os).

-spec type(fifo:dataset_id(), zone | kvm) ->
                  ok | {error, no_servers}.
?HS(type).

-spec zone_type(fifo:dataset_id(), lx | ipkg | lipkg) ->
                       ok | {error, no_servers}.
?HS(zone_type).

-spec users(fifo:dataset_id(), list()) ->
                   ok | {error, no_servers}.
?HS(users).

-spec version(fifo:dataset_id(), binary()) ->
                     ok | {error, no_servers}.
?HS(version).

-spec kernel_version(fifo:dataset_id(), binary()) ->
                            ok | {error, no_servers}.
?HS(kernel_version).

-spec set_metadata(fifo:dataset_id(), fifo:attr_list()) ->
                          ok | {error, no_servers}.
?HS(set_metadata).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec send(MSG::fifo:sniffle_dataset_message()) ->
                  ok |
                  atom() |
                  {ok, Reply::term()} |
                  {error, no_servers}.
send(Msg) ->
    send(mdns, Msg).


-spec send(mdns | {term(), term()},
           MSG::fifo:sniffle_dataset_message()) ->
                  ok |
                  atom() |
                  {ok, Reply::term()} |
                  {error, no_servers}.

send(Sniffle, Msg) ->
    case libsniffle_server:send(Sniffle, Msg) of
        {reply, Reply} ->
            Reply;
        noreply ->
            ok;
        E ->
            E
    end.
