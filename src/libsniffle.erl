-module(libsniffle).

-export([
         start/0,
         servers/0,
         version/0,
         cloud_status/0
        ]).

%%%===================================================================
%%% Generatl Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the libsniffle application
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | error.
start() ->
    application:start(mdns_client_lib),
    application:start(libsniffle).

%%--------------------------------------------------------------------
%% @doc Lists the currently available servers.
%% @end
%%--------------------------------------------------------------------
-spec servers() -> [{{string(),
                      [{atom(), binary()}]},
                     string(),integer()}].
servers() ->
    libsniffle_server:servers().

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches version
%% @spec version() -> binary
%% @end
%%--------------------------------------------------------------------
-spec version() -> binary() |
                   {error, no_servers}.
version() ->
    ServerVersion = send(version),
    ServerVersion.

%%--------------------------------------------------------------------
%% @doc Reads the overall cloud status.
%% @end
%%--------------------------------------------------------------------
-spec cloud_status() -> {'error','no_servers'} |
                        {ok, {Resources::fifo:object(),
                              Warnings::fifo:object()}}.
cloud_status() ->
    send({cloud, status}).

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
