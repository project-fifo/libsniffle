-module(ls_img).

-export([
         create/4,
         delete/1,
         delete/2,
         get/2,
         list/0,
         list/1
        ]).

-define(UUID, <<UUID:36/binary>>).

%%%===================================================================
%%%  IMG Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new image part on the server.
%% @end
%%--------------------------------------------------------------------
-spec create(Img::fifo:dataset_id(), Idx::integer()|done,
                 Data::binary(), Ref::term()) ->
                        {ok, Ref1::term()} |
                        {'error','no_servers'}.
create(?UUID, Idx, Data, Ref) when Idx >= 0,
                                  is_binary(Data) ->
    send({img, create, UUID, Idx, Data, Ref}).

%%--------------------------------------------------------------------
%% @doc Deletes an entire image form the server
%% @end
%%--------------------------------------------------------------------
-spec delete(Img::fifo:dataset_id()) ->
                        ok | not_found |
                        {'error','no_servers'}.
delete(?UUID) ->
    send({img, delete, UUID}).

%%--------------------------------------------------------------------
%% @doc Deletes a image part from the server
%% @end
%%--------------------------------------------------------------------
-spec delete(Img::fifo:dataset_id(), Idx::integer()) ->
                        ok | not_found |
                        {'error','no_servers'}.
delete(?UUID, Idx) when Idx >= 0 ->
    send({img, delete, UUID, Idx}).

%%--------------------------------------------------------------------
%% @doc Reads a image part from the server
%% @end
%%--------------------------------------------------------------------
-spec get(Img::fifo:dataset_id(), Idx::integer()) ->
                     {'error','no_servers'} |
                     not_found |
                     {ok, binary()}.
get(?UUID, Idx) when Idx >= 0 ->
    send({img, get, UUID, Idx}).

%%--------------------------------------------------------------------
%% @doc Lists all images on the server.
%% @end
%%--------------------------------------------------------------------
-spec list() ->
                      {ok, Imgs::[fifo:uuid()]} |
                      {'error','no_servers'}.
list() ->
    send({img, list}).

%%--------------------------------------------------------------------
%% @doc Lists all parts for a images on the server.
%% @end
%%--------------------------------------------------------------------
-spec list(Img::fifo:dataset_id()) ->
                      {ok, Parts::[pos_integer()]} |
                      {'error','no_servers'}.
list(?UUID) ->
    send({img, list, ?UUID}).

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
