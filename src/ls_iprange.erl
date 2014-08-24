-module(ls_iprange).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         create/8,
         delete/1,
         get/1,
         release/2,
         claim/1,
         list/2,
         list/3
        ]).

-export([
         name/2,
         uuid/2,
         network/2,
         netmask/2,
         gateway/2,
         set_metadata/2,
         tag/2,
         vlan/2
        ]).


-export([ip_to_bin/1,
         ip_to_int/1]).

%%%===================================================================
%%%  Iprange Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new IP range on the server. All ips are passed as
%%   integer or binary.
%% @end
%%--------------------------------------------------------------------
-spec create(Iprange::binary(),
             Network::integer() | binary(),
             Gateway::integer() | binary(),
             Netmask::integer() | binary(),
             First::integer() | binary(),
             Last::integer() | binary(),
             Tag::binary(),
             Vlan::pos_integer()) ->
                    {ok, UUID::fifo:iprange_id()} |
                    duplicate |
                    {'error','no_servers'}.

create(Iprange, Network, Gateway, Netmask, First, Last, Tag, Vlan) when
      is_binary(Iprange),
      is_binary(Tag),
      is_integer(Network),
      is_integer(Gateway), Network =:= (Gateway band Netmask),
      is_integer(Netmask),
      is_integer(First), Network =:= (First band Netmask),
      is_integer(Last), Network =:= (Last band Netmask),
      is_integer(Vlan), Vlan >= 0 ->
    send({iprange, create, Iprange,
          Network, Gateway, Netmask,
          First, Last,
          Tag, Vlan});

create(Iprange, Network, Gateway, Netmask, First, Last, Tag, Vlan) when
      is_binary(Iprange),
      is_binary(Tag),
      is_integer(Vlan), Vlan >= 0->
    create(Iprange,
           ip_to_int(Network),
           ip_to_int(Gateway),
           ip_to_int(Netmask),
           ip_to_int(First),
           ip_to_int(Last),
           Tag,
           Vlan).

%%--------------------------------------------------------------------
%% @doc Deletes a iprange from the server.
%% @end
%%--------------------------------------------------------------------
-spec delete(Iprange::binary()) ->
                    ok | not_found |
                    {'error','no_servers'}.

delete(Iprange) ->
    send({iprange, delete, Iprange}).

%%--------------------------------------------------------------------
%% @doc Reads all the info of a iprange from the server.
%% @end
%%--------------------------------------------------------------------
-spec get(Iprange::binary()) ->
                 not_found |
                 {ok, fifo:iprange()} |
                 {'error','no_servers'}.

get(Iprange) ->
    send({iprange, get, Iprange}).

%%--------------------------------------------------------------------
%% @doc Tries to release a claimed ip address from a range.
%% @end
%%--------------------------------------------------------------------
-spec release(Iprange::fifo:iprange_id(),
              Ip::integer()) ->
                     ok | not_found |
                     {'error','no_servers'}.

release(Iprange, Ip) when
      is_binary(Iprange),
      is_integer(Ip) ->
    send({iprange, release, Iprange, ip_to_int(Ip)});

release(Iprange, Ip) when
      is_binary(Iprange) ->
    release(Iprange, ip_to_int(Ip)).

%%--------------------------------------------------------------------
%% @doc Claims a ipaddress and returns the address and the relevant
%%   network information.
%% @end
%%--------------------------------------------------------------------
-spec claim(Iprange::fifo:iprange_id()) ->
                   not_found |
                   {ok, {Tag::binary(),
                         IP::pos_integer(),
                         Netmask::pos_integer(),
                         Gateway::pos_integer()}} |
                   {error, failed} |
                   {'error','no_servers'}.
claim(Iprange) ->
    send({iprange, claim, Iprange}).

%%--------------------------------------------------------------------
%% @doc Lists all ip ranges known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Reqs::[fifo:matcher()], boolean()) ->
                  {ok, [{Ranking::integer(), ID::fifo:iprange_id()}]} |
                  {ok, [{Ranking::integer(), ID::fifo:iprange()}]} |
                  {'error','no_servers'}.
list(Reqs, Full) ->
    list(mdns, Reqs, Full).

list(Sniffle, Reqs, Full) ->
    send(Sniffle, {iprange, list, Reqs, Full}).

%%%===================================================================
%%% Utility functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This function convers an integer to the human readable ip
%%   format as <pre>AAA.BBB.CCC.DDD</pre>.
%% @end
%%--------------------------------------------------------------------
-spec ip_to_bin(integer()) -> binary().

ip_to_bin(IP) ->
    <<A, B, C, D>> = <<IP:32>>,
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D])).

%%--------------------------------------------------------------------
%% @doc This function an human readable ip format
%%    <pre>AAA.BBB.CCC.DDD</pre> to an integer.
%% @end
%%--------------------------------------------------------------------
-spec ip_to_int(integer()|string()|binary()) -> integer().

ip_to_int(IP) when is_integer(IP) ->
    IP;

ip_to_int(IP) ->
    [As, Bs, Cs, Ds] = re:split(IP, "\\.", [{return, list}]),
    {A, _} = string:to_integer(As),
    {B, _} = string:to_integer(Bs),
    {C, _} = string:to_integer(Cs),
    {D, _} = string:to_integer(Ds),
    <<I:32>> = <<A:8, B:8, C:8, D:8>>,
    I.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec send(MSG::fifo:sniffle_iprange_message()) ->
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

-define(HS(F),
        F(DTRace, Val) ->
               send({iprange, F, DTRace, Val})).

?HS(name).
?HS(uuid).
?HS(network).
?HS(netmask).
?HS(gateway).
?HS(set_metadata).
?HS(tag).
?HS(vlan).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
ip_convert_test() ->
    IP1 = <<"192.168.0.0">>,
    ?assertEqual(ip_to_bin(ip_to_int(IP1)), IP1).

ip_to_int_test() ->
    ?assertEqual(ip_to_int("255.255.255.255"), 16#FFFFFFFF),
    ?assertEqual(ip_to_int("0.255.255.255"), 16#00FFFFFF),
    ?assertEqual(ip_to_int("255.0.255.255"), 16#FF00FFFF),
    ?assertEqual(ip_to_int("255.255.0.255"), 16#FFFF00FF),
    ?assertEqual(ip_to_int("255.255.255.0"), 16#FFFFFF00),
    ?assertEqual(ip_to_int("255.255.255.240"), 16#FFFFFFF0).

ip_to_bin_test() ->
    ?assertEqual(ip_to_bin(16#FFFFFFFF), <<"255.255.255.255">>),
    ?assertEqual(ip_to_bin(16#00FFFFFF), <<"0.255.255.255">>),
    ?assertEqual(ip_to_bin(16#FF00FFFF), <<"255.0.255.255">>),
    ?assertEqual(ip_to_bin(16#FFFF00FF), <<"255.255.0.255">>),
    ?assertEqual(ip_to_bin(16#FFFFFF00), <<"255.255.255.0">>),
    ?assertEqual(ip_to_bin(16#FFFFFFF0), <<"255.255.255.240">>).
-endif.
