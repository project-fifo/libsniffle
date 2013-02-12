-module(libsniffle).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         start/0,
         servers/0,
         create/3,
         version/0,
         dtrace/1
        ]).

-export([
         vm_register/2,
         vm_unregister/1,
         vm_update/3,
         vm_set/2,
         vm_set/3,
         vm_log/2,
         vm_snapshot/2,
         vm_delete_snapshot/2,
         vm_rollback_snapshot/2,
         vm_list/0,
         vm_list/1,
         vm_get/1,
         vm_start/1,
         vm_stop/1,
         vm_reboot/1,
         vm_delete/1
        ]).

-export([
         hypervisor_register/3,
         hypervisor_unregister/1,
         hypervisor_get/1,
         hypervisor_set/2,
         hypervisor_set/3,
         hypervisor_list/0,
         hypervisor_list/1
        ]).

-export([
         dataset_create/1,
         dataset_delete/1,
         dataset_get/1,
         dataset_set/2,
         dataset_set/3,
         dataset_list/0,
         dataset_list/1
        ]).

-export([
         package_create/1,
         package_delete/1,
         package_get/1,
         package_set/2,
         package_set/3,
         package_list/0,
         package_list/1
        ]).

-export([
         iprange_create/8,
         iprange_delete/1,
         iprange_get/1,
         iprange_release/2,
         iprange_claim/1,
         iprange_list/0,
         iprange_list/1,
         iprange_set/2,
         iprange_set/3
        ]).

-export([ip_to_bin/1,
         ip_to_int/1]).

-export([cloud_status/0]).




%%%===================================================================
%%% Generatl Functions
%%%===================================================================

dtrace(Script) ->
    case libsniffle_server:get_server() of
        {error, no_server} ->
            {error, no_server};
        {ok, Server, Port} ->
            case gen_tcp:connect(Server, Port, [binary, {active, true}, {packet, 4}]) of
                {ok, Socket} ->
                    ok = gen_tcp:send(Socket, term_to_binary({trace, Script})),
                    {ok, Socket};
                E ->
                    E
            end
    end.

-spec start() -> ok | error.
start() ->
    application:start(mdns_client_lib),
    application:start(libsniffle).

-spec servers() -> [{{string(),
                      [{atom(), binary()}]},
                     string(),integer()}].
servers() ->
    libsniffle_server:servers().

-spec create(PackageID::binary(), DatasetID::binary(), Config::[{Key::binary(), Value::term()}]) ->
                    {error, no_servers} |
                    {ok, UUID::binary()}.

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches version
%% @spec version() -> binary
%% @end
%%--------------------------------------------------------------------

-spec version() -> {binary(), binary()} |
                   not_found |
                   {error, no_servers}.
version() ->
    ServerVersion = send(version),
    ServerVersion.


create(PackageID, DatasetID, Config) ->
    send({vm, create, PackageID, DatasetID, Config}).
%%%===================================================================
%%% VM Functions
%%%===================================================================

-spec vm_register(VM::fifo:uuid(), Hypervisor::fifo:hypervisor()) -> ok |
                                                                     {'error','no_servers'}.
vm_register(VM, Hypervisor) when
      is_binary(VM),
      is_binary(Hypervisor) ->
    send({vm, register, VM, Hypervisor}).

-spec vm_unregister(VM::fifo:uuid()) -> ok |
                                        not_found |
                                        {'error','no_servers'}.
vm_unregister(VM) when
      is_binary(VM) ->
    send({vm, unregister, VM}).

-spec vm_start(VM::fifo:uuid()) -> {ok, ok} |
                                   {'error','no_servers'}.
vm_start(VM) when
      is_binary(VM) ->
    send({vm, start, VM}).

-spec vm_stop(VM::fifo:uuid()) -> ok | not_found |
                                  {'error','no_servers'}.
vm_stop(VM) when
      is_binary(VM)->
    send({vm, stop, VM}).

-spec vm_get(VM::fifo:uuid()) ->
                    not_found |
                    {ok, term()} |
                    {'error','no_servers'}.
vm_get(VM) when
      is_binary(VM) ->
    send({vm, get, VM}).

-spec vm_reboot(VM::fifo:uuid()) -> ok | not_found |
                                    {'error','no_servers'}.
vm_reboot(VM) when
      is_binary(VM) ->
    send({vm, reboot, VM}).

-spec vm_delete(VM::fifo:uuid()) -> ok | not_found |
                                    {'error','no_servers'}.
vm_delete(VM) when
      is_binary(VM) ->
    send({vm, delete, VM}).

-spec vm_update(VM::fifo:uuid(),
                Package::binary() | undefined,
                Config::fifo:config_list()) -> ok | not_found |
                              {'error','no_servers'}.
vm_update(VM, Package, Config) when
      is_binary(VM),
      is_list(Config) ->
    send({vm, update, VM, Package, Config}).

-spec vm_set(VM::fifo:uuid(),
             Attribute::binary(),
             Value::any()) -> ok | not_found |
                              {'error','no_servers'}.
vm_set(VM, Attribute, Value) when
      is_binary(VM) ->
    send({vm, set, VM, Attribute, Value}).

-spec vm_set(VM::fifo:uuid(),
             Attributes::fifo:config_list()) -> ok | not_found |
                                                {'error','no_servers'}.
vm_set(VM, Attributes) when
      is_binary(VM) ->
    send({vm, set, VM, [{K, V} || {K, V} <- Attributes,
                                             is_binary(K)]}).

-spec vm_log(Vm::fifo:uuid(), Log::binary()) -> ok |
                                                {'error','no_servers'}.
vm_log(Vm, Log) ->
    send({vm, log, Vm, Log}).

-spec vm_snapshot(Vm::fifo:uuid(), Comment::binary()) -> {ok, fifo:uuid()} |
                                                         {'error','no_servers'}.

vm_snapshot(Vm, Comment) ->
    send({vm, snapshot, Vm, Comment}).

-spec vm_delete_snapshot(Vm::fifo:uuid(),
                         UUID::binary()) -> {ok, fifo:uuid()} |
                                            {'error','no_servers'}.

vm_delete_snapshot(Vm, UUID) ->
    send({vm, snapshot, delete, Vm, UUID}).


-spec vm_rollback_snapshot(Vm::fifo:uuid(),
                           UUID::binary()) -> {ok, fifo:uuid()} |
                                            {'error','no_servers'}.

vm_rollback_snapshot(Vm, UUID) ->
    send({vm, snapshot, rollback, Vm, UUID}).

-spec vm_list() -> {ok, [fifo:uuid()]} |
                   {'error','no_servers'}.
vm_list() ->
    send({vm, list}).

-spec vm_list(Reqs::[fifo:matcher()]) ->
                     {ok, [fifo:uuid()]} |
                     {'error','no_servers'}.
vm_list(Reqs) ->
    send({vm, list, Reqs}).

%%%===================================================================
%%% Hypervisor Functions
%%%===================================================================

-spec hypervisor_register(Hypervisor::binary(), Host::inet:ip_address() | inet:hostname(), Port::inet:port_number()) ->
                                 ok |
                                 {'error','no_servers'}.
hypervisor_register(Hypervisor, Host, Port) when
      is_binary(Hypervisor),
      is_integer(Port),
      Port > 0 ->
    send({hypervisor, register, Hypervisor, Host, Port}).

-spec hypervisor_unregister(Hypervisor::binary()) -> ok | not_found |
                                                     {'error','no_servers'}.
hypervisor_unregister(Hypervisor) ->
    send({hypervisor, unregister, Hypervisor}).

-spec hypervisor_get(Hypervisor::binary()) ->
                            not_found |
                            {ok, fifo:hypervisor()} |
                            {'error','no_servers'}.
hypervisor_get(Hypervisor) ->
    send({hypervisor, get, Hypervisor}).

-spec hypervisor_set(Hypervisor::binary(), Resource::binary(), Value::fifo:value()) ->
                                     ok |
                                     not_found |
                                     {'error','no_servers'}.
hypervisor_set(Hypervisor, Resource, Value) ->
    send({hypervisor, set, Hypervisor, Resource, Value}).

-spec hypervisor_set(Hypervisor::binary(), Resources::fifo:config_list()) ->
                                     ok | not_found |
                                     {'error','no_servers'}.
hypervisor_set(Hypervisor, Resources) ->
    send({hypervisor, set, Hypervisor, Resources}).

-spec hypervisor_list() -> {ok, [binary()]} |
                           {'error','no_servers'}.
hypervisor_list() ->
    send({hypervisor, list}).

-spec hypervisor_list(Requirements::[fifo:matcher()]) -> {ok, [binary()]} |
                                                         {'error','no_servers'}.
hypervisor_list(Requirements) ->
    send({hypervisor, list, Requirements}).

%%%===================================================================
%%%  DATASET Functions
%%%===================================================================

-spec dataset_create(Dataset::binary()) -> ok | doublicate |
                                           {'error','no_servers'}.
dataset_create(Dataset) ->
    send({dataset, create, Dataset}).

-spec dataset_delete(Dataset::binary()) -> ok | not_found |
                                           {'error','no_servers'}.
dataset_delete(Dataset) ->
    send({dataset, delete, Dataset}).

-spec dataset_get(Dataset::binary()) -> {'error','no_servers'} |
                                        not_found |
                                        {ok, [{Key::term(), Key::term()}]}.
dataset_get(Dataset) ->
    send({dataset, get, Dataset}).

-spec dataset_set(Dataset::binary(), Attirbutes::[{Key::term(), Value::term()}]) ->
                         ok |
                         not_found |
                         {'error','no_servers'}.
dataset_set(Dataset, Attributes) ->
    send({dataset, set, Dataset, Attributes}).

-spec dataset_set(Dataset::binary(), Attribute::term(), Value::term()) ->
                         ok | not_found |
                         {'error','no_servers'}.
dataset_set(Dataset, Attribute, Value) ->
    send({dataset, set, Dataset, Attribute, Value}).

-spec dataset_list() -> {ok, Datasets::[binary()]} | {'error','no_servers'}.
dataset_list() ->
    send({dataset, list}).

-spec dataset_list(Reqs::term()) -> {ok, Datasets::[binary()]} | {'error','no_servers'}.
dataset_list(Reqs) ->
    send({dataset, list, Reqs}).

%%%===================================================================
%%%  PACKAGE Functions
%%%===================================================================

-spec package_create(Package::binary()) ->
                            ok | not_found |
                            {'error','no_servers'}.
package_create(Package) when
      is_binary(Package) ->
    send({package, create, Package}).

-spec package_delete(Package::binary()) ->
                            ok | not_found |
                            {'error','no_servers'}.
package_delete(Package) when
      is_binary(Package) ->
    send({package, delete, Package}).

-spec package_get(Package::binary()) ->
                         not_found |
                         {ok, Package::term()} |
                         {'error','no_servers'}.
package_get(Package) when
      is_binary(Package) ->
    send({package, get, Package}).

-spec package_set(Package::binary(),
                  Attirbutes::fifo:config_list()) -> ok | not_found |
                                                     {'error','no_servers'}.
package_set(Package, Attributes) when
      is_binary(Package),
      is_list(Attributes) ->
    send({package, set, Package, Attributes}).

-spec package_set(Package::binary(),
                  Attribute::binary(),
                  Value::fifo:value()) -> ok | not_found |
                                          {'error','no_servers'}.
package_set(Package, Attribute, Value)  when
      is_binary(Package) ->
    send({package, set, Package, Attribute, Value}).

-spec package_list() -> {ok, Packages::[binary()]} |
                        {'error','no_servers'}.
package_list() ->
    send({package, list}).

-spec package_list(Reqs::[fifo:matcher()]) -> {ok, Packages::[binary()]} |
                                              {'error','no_servers'}.
package_list(Reqs) ->
    send({package, list, Reqs}).

%%%===================================================================
%%%  PACKAGE Functions
%%%===================================================================

-spec iprange_create(Iprange::binary(),
                     Network::integer() | string() | binary(),
                     Gateway::integer() | string() | binary(),
                     Netmask::integer() | string() | binary(),
                     First::integer() | string() | binary(),
                     Last::integer() | string() | binary(),
                     Tag::binary(),
                     Vlan::pos_integer()) ->
                            ok | doublicate |
                            {'error','no_servers'}.

iprange_create(Iprange, Network, Gateway, Netmask, First, Last, Tag, Vlan) when
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

iprange_create(Iprange, Network, Gateway, Netmask, First, Last, Tag, Vlan) when
      is_binary(Iprange),
      is_binary(Tag),
      is_integer(Vlan), Vlan >= 0->
    iprange_create(Iprange,
                   ip_to_int(Network),
                   ip_to_int(Gateway),
                   ip_to_int(Netmask),
                   ip_to_int(First),
                   ip_to_int(Last),
                   Tag,
                   Vlan).

-spec iprange_delete(Iprange::binary()) ->
                            ok | not_found |
                            {'error','no_servers'}.

iprange_delete(Iprange) ->
    send({iprange, delete, Iprange}).

-spec iprange_get(Iprange::binary()) ->
                         not_found |
                         {ok, term()} |
                         {'error','no_servers'}.

iprange_get(Iprange) ->
    send({iprange, get, Iprange}).

-spec iprange_release(Iprange::binary(),
                      Ip::integer() | string() | binary()) ->
                             ok | not_found |
                             {'error','no_servers'}.

iprange_release(Iprange, Ip) when
      is_binary(Iprange),
      is_integer(Ip) ->
    send({iprange, release, Iprange, ip_to_int(Ip)});

iprange_release(Iprange, Ip) when
      is_binary(Iprange) ->
    iprange_release(Iprange, ip_to_int(Ip)).

-spec iprange_claim(Iprange::binary()) ->
                           not_found |
                           {ok, integer()} |
                           {'error','no_servers'}.

iprange_claim(Iprange) ->
    send({iprange, claim, Iprange}).

-spec iprange_list() -> {ok, [binary()]} |
                        {'error','no_servers'}.

iprange_list() ->
    send({iprange, list}).

-spec iprange_list(Reqs::[fifo:matcher()]) -> {ok, [binary()]} |
                                              {'error','no_servers'}.
iprange_list(Reqs) ->
    send({iprange, list, Reqs}).


-spec iprange_set(Iprange::binary(),
                  Attirbutes::fifo:config_list()) -> ok | not_found |
                                                     {'error','no_servers'}.
iprange_set(Iprange, Attributes) when
      is_binary(Iprange),
      is_list(Attributes) ->
    send({iprange, set, Iprange, Attributes}).

-spec iprange_set(Iprange::binary(),
                  Attribute::binary()|[binary()],
                  Value::fifo:value()) -> ok | not_found |
                                          {'error','no_servers'}.
iprange_set(Iprange, Attribute, Value)  when
      is_binary(Iprange) ->
    send({iprange, set, Iprange, Attribute, Value}).

-spec cloud_status() -> {'error','no_servers'} |
                        {Resources::fifo:config_list(),
                         Warnings::fifo:config_list()}.

cloud_status() ->
    send({cloud, status}).



ip_to_bin(IP) ->
    <<A, B, C, D>> = <<IP:32>>,
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D])).

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
-spec send(MSG::fifo:sniffle_message()) ->
                  atom() |
                  {ok, Reply::term()} |
                  {error, no_servers}.
send(Msg) ->
    case libsniffle_server:send(Msg) of
        {reply, Reply} ->
            Reply;
        noreply ->
            ok;
        E ->
            E
    end.

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
