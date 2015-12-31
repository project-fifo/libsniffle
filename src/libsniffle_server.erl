%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 20 Aug 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(libsniffle_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         send/2,
         stream/2,
         servers/0]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         get_server/0
        ]).

-define(SERVER, ?MODULE).

-record(state, {zmq_worker}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send(Sniffle, Msg) ->
    gen_server:call(?SERVER, {send, Sniffle, Msg}).

stream(Msg, StreamFn) ->
    gen_server:call(?SERVER, {stream, Msg, StreamFn}).

get_server() ->
    gen_server:call(?SERVER, get_server).

servers() ->
    gen_server:call(?SERVER, servers).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([]) ->
    {ok, Pid} = mdns_client_lib:instance("sniffle"),
    {ok, #state{zmq_worker = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(get_server, _From, #state{zmq_worker = Pid} = State) ->
    Reply = mdns_client_lib_server:get_server(Pid),
    {reply, Reply, State};

handle_call(servers, _From, #state{zmq_worker = Pid} = State) ->
    Reply = mdns_client_lib:servers(Pid),
    {reply, Reply, State};

handle_call({send, mdns, Msg}, _From, #state{zmq_worker = Pid} = State) ->
    Reply = mdns_client_lib:call(Pid, Msg),
    {reply, Reply, State};

handle_call({stream, Msg, StreamFn}, _From, #state{zmq_worker = Pid} = State) ->
    Reply = mdns_client_lib:stream(Pid, Msg, StreamFn, 60000),
    {reply, Reply, State};

handle_call({send, {IP, Port}, Msg}, _From, State) ->
    Opts = [binary, {active, false}, {packet, 4}],
    case gen_tcp:connect(IP, Port, Opts, 100) of
        {ok, Socket} ->
            R = case gen_tcp:send(Socket, term_to_binary(Msg)) of
                    ok ->
                        recv(Socket);
                    _ ->
                        {error, send}
                end,
            gen_tcp:close(Socket),
            {reply, R, State};
        _ ->
            {reply, {error, connect}, State}
    end;


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


recv(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Repl} ->
            case binary_to_term(Repl) of
                {reply, Reply} ->
                    Reply;
                _ ->
                    {error, reply}
            end;
        _ ->
            {error, recv}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
