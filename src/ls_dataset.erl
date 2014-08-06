-module(ls_dataset).

-export([
         create/1,
         import/1,
         delete/1,
         get/1,
         list/0,
         list/1,
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
                            {ok, UUID::fifo:uuid()} |
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
                         {ok, [{Key::term(), Key::term()}]}.
get(Dataset) ->
    send({dataset, get, Dataset}).

%%--------------------------------------------------------------------
%% @doc Lists all datasets known to the system.
%% @end
%%--------------------------------------------------------------------
-spec list() ->
                          {ok, Datasets::[binary()]} |
                          {'error','no_servers'}.
list() ->
    send({dataset, list}).

%%--------------------------------------------------------------------
%% @doc Lists all datasets known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Reqs::term()) ->
                          {ok, Datasets::[{Ranking::integer(),
                                           ID::fifo:id()}]} |
                          {'error','no_servers'}.
list(Reqs) ->
    send({dataset, list, Reqs}).

%%--------------------------------------------------------------------
%% @doc Lists all datasets known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Reqs::term(), boolean()) ->
                          {ok, Datasets::[{Ranking::integer(),
                                           ID::fifo:id()|fifo:object()}]} |
                          {'error','no_servers'}.
list(Reqs, Full) ->
    send({dataset, list, Reqs, Full}).

-define(HS(F),
        F(Dataset, Val) ->
               send({dataset, F, Dataset, Val})).

?HS(status).
?HS(imported).
?HS(description).
?HS(disk_driver).
?HS(homepage).
?HS(image_size).
?HS(name).
?HS(networks).
?HS(nic_driver).
?HS(os).
?HS(type).
?HS(users).
?HS(version).
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
