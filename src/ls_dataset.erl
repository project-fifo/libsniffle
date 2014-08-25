-module(ls_dataset).

-export([
         create/1,
         import/1,
         delete/1,
         get/1,
         list/0,
         list/2
        ]).

-export([
         set_metadata/2,
         imported/2,
         status/2,
         description/2,
         disk_driver/2,
         homepage/2,
         image_size/2,
         name/2,
         networks/2,
         nic_driver/2,
         os/2,
         type/2,
         users/2,
         version/2
        ]).

%%%===================================================================
%%%  DATASET Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new dataset with the given UUID.
%% @end
%%--------------------------------------------------------------------
-spec create(Dataset::binary()) ->
                    ok | duplicate |
                    {'error','no_servers'}.
create(Dataset) ->
    send({dataset, create, Dataset}).

%%--------------------------------------------------------------------
%% @doc Tries to import a dataset from a dsapi server URL.
%% @end
%%--------------------------------------------------------------------
-spec import(URL::binary()) ->
                    {ok, UUID::fifo:dataset_id()} |
                    {error, Reason::term()} |
                    {'error','no_servers'}.
import(URL) ->
    send({dataset, import, URL}).

%%--------------------------------------------------------------------
%% @doc Deletes a dataset from the server, this will also delete the
%%   image from the database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Dataset::binary()) ->
                    ok | not_found |
                    {'error','no_servers'}.
delete(Dataset) ->
    send({dataset, delete, Dataset}).

%%--------------------------------------------------------------------
%% @doc Gets a dataset from the database.
%% @end
%%--------------------------------------------------------------------
-spec get(Dataset::binary()) ->
                 {'error','no_servers'} |
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
                  {'error','no_servers'}.
list() ->
    send({dataset, list}).

%%--------------------------------------------------------------------
%% @doc Lists all datasets known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Reqs::term(), boolean()) ->
                  {ok, Datasets::[{Ranking::integer(), ID::fifo:dataset_id()}]} |
                  {ok, Datasets::[{Ranking::integer(), Dset::fifo:dataset()}]} |
                  {'error','no_servers'}.
list(Reqs, Full) ->
    send({dataset, list, Reqs, Full}).

-define(HS(F),
        F(Dataset, Val) ->
               send({dataset, F, Dataset, Val})).

-spec status(fifo:dataset_id(), binary()) ->
                    ok | {error, no_servers}.
?HS(status).

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

-spec networks(fifo:dataset_id(), list()) ->
                    ok | {error, no_servers}.
?HS(networks).

-spec nic_driver(fifo:dataset_id(), binary()) ->
                        ok | {error, no_servers}.
?HS(nic_driver).

-spec os(fifo:dataset_id(), binary()) ->
                ok | {error, no_servers}.
?HS(os).

-spec type(fifo:dataset_id(), zone | kvm) ->
                    ok | {error, no_servers}.
?HS(type).

-spec users(fifo:dataset_id(), list()) ->
                    ok | {error, no_servers}.
?HS(users).

-spec version(fifo:dataset_id(), binary()) ->
                    ok | {error, no_servers}.
?HS(version).

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
send(Sniffle, Msg) ->
    case libsniffle_server:send(Sniffle, Msg) of
        {reply, Reply} ->
            Reply;
        noreply ->
            ok;
        E ->
            E
    end.
