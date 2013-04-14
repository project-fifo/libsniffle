-module(libsniffle).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         start/0,
         servers/0,
         create/3,
         version/0
        ]).

-export([
         dtrace_add/2,
         dtrace_delete/1,
         dtrace_get/1,
         dtrace_set/2,
         dtrace_set/3,
         dtrace_list/1,
         dtrace_list/0,
         dtrace_run/2
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
         vm_stop/2,
         vm_reboot/2,
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
         dataset_import/1,
         dataset_delete/1,
         dataset_get/1,
         dataset_set/2,
         dataset_set/3,
         dataset_list/0,
         dataset_list/1
        ]).

-export([
         img_create/3,
         img_delete/1,
         img_delete/2,
         img_get/2,
         img_list/0,
         img_list/1
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
%%% DTrace Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds a new dtrace script to sniffle, the name is a plain
%%   binary while the script is a string that can contain placeholders
%%   encapsulated in $ signs.
%%   A UUID is returned.
%% @end
%%--------------------------------------------------------------------
-spec dtrace_add(Name::binary(),
                 Script::list()) ->
                        {ok, UUID::fifo:uuid()} |
                        duplicate |
                        {'error','no_servers'}.
dtrace_add(Name, Script) when
      is_binary(Name),
      is_list(Script)->
    send({dtrace, add, Name, Script}).

%%--------------------------------------------------------------------
%% @doc Deletes a dtrace script from the library
%% @end
%%--------------------------------------------------------------------
-spec dtrace_delete(UUID::fifo:uuid()) ->
                           ok |
                           {'error','no_servers'}.
dtrace_delete(ID) when
      is_binary(ID)->
    send({dtrace, delete, ID}).

%%--------------------------------------------------------------------
%% @doc Reads a dtrace script and returns the jsx object for it.
%% @end
%%--------------------------------------------------------------------
-spec dtrace_get(UUID::fifo:uuid()) ->
                        {ok, Data::fifo:config_list()} |
                        not_found |
                        {'error','no_servers'}.
dtrace_get(ID) when
      is_binary(ID)->
    send({dtrace, get, ID}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database.
%% @end
%%--------------------------------------------------------------------
-spec dtrace_list() ->
                         {ok, [UUID::fifo:uuid()]} |
                         {'error','no_servers'}.
dtrace_list()->
    send({dtrace, list}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec dtrace_list([Requirement::fifo:matcher()]) ->
                         {ok, [UUID::fifo:uuid()]} |
                         {'error','no_servers'}.
dtrace_list(Requirements)->
    send({dtrace, list, Requirements}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec dtrace_set(Dtrace::fifo:uuid(),
                 Attribute::fifo:keys(),
                 Value::fifo:value() | delete) ->
                        ok | not_found |
                        {'error','no_servers'}.
dtrace_set(DTrace, Attribute, Value) when
      is_binary(DTrace) ->
    send({dtrace, set, DTrace, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets options on a dtace script. The root key 'config' has a
%%   special meaning here since it holds replacement variables.
%% @end
%%--------------------------------------------------------------------
-spec dtrace_set(DTrace::fifo:uuid(),
                 Attributes::fifo:config_list()) ->
                        ok | not_found |
                        {'error','no_servers'}.
dtrace_set(DTrace, Attributes) when
      is_binary(DTrace) ->
    send({dtrace, set, DTrace, Attributes}).

%%--------------------------------------------------------------------
%% @doc Starts a dtrace script on the given hypervisors, returns an
%%   opened tcp socket that will stream incoming data every second and
%%   allows some interaction.
%% @end
%%--------------------------------------------------------------------
-spec dtrace_run(DTrace::fifo:uuid(),
                 Servers::[fifo:hypervisor()]) ->
                        {ok, Socket::port()} |
                        not_found |
                        {'error','no_servers'}.

dtrace_run(ID, Servers) when
      is_binary(ID)->
    case libsniffle_server:get_server() of
        {error, no_server} ->
            {error, no_server};
        {ok, Server, Port} ->
            case gen_tcp:connect(Server, Port, [binary, {active, true}, {packet, 4}], 100) of
                {ok, Socket} ->
                    ok = gen_tcp:send(Socket, term_to_binary({dtrace, run, ID, Servers})),
                    {ok, Socket};
                E ->
                    E
            end
    end.

%%%===================================================================
%%% VM Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initiates the creating a new VM, this will just return the
%%   UUID progress is directy written to the object in the database.
%% @end
%%--------------------------------------------------------------------
-spec create(PackageID::binary(), DatasetID::binary(), Config::[{Key::binary(), Value::term()}]) ->
                    {error, no_servers} |
                    {ok, UUID::binary()}.

create(PackageID, DatasetID, Config) ->
    send({vm, create, PackageID, DatasetID, Config}).

%%--------------------------------------------------------------------
%% @doc Registeres an existing VM with sniffle.
%% @end
%%--------------------------------------------------------------------
-spec vm_register(VM::fifo:uuid(), Hypervisor::fifo:hypervisor_id()) ->
                         ok |
                         {'error','no_servers'}.
vm_register(VM, Hypervisor) when
      is_binary(VM),
      is_binary(Hypervisor) ->
    send({vm, register, VM, Hypervisor}).

%%--------------------------------------------------------------------
%% @doc Unregisteres a VM from the database, this does not include
%%   deleting it from the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec vm_unregister(VM::fifo:uuid()) ->
                           ok |
                           not_found |
                           {'error','no_servers'}.
vm_unregister(VM) when
      is_binary(VM) ->
    send({vm, unregister, VM}).

%%--------------------------------------------------------------------
%% @doc Reads the VM attribute from the database.
%% @end
%%--------------------------------------------------------------------
-spec vm_get(VM::fifo:uuid()) ->
                    not_found |
                    {ok, term()} |
                    {'error','no_servers'}.
vm_get(VM) when
      is_binary(VM) ->
    send({vm, get, VM}).

%%--------------------------------------------------------------------
%% @doc Starts a VM on the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec vm_start(VM::fifo:uuid()) ->
                      ok | not_found |
                      {'error','no_servers'}.
vm_start(VM) when
      is_binary(VM) ->
    send({vm, start, VM}).

%%--------------------------------------------------------------------
%% @doc Stops a VM on the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec vm_stop(VM::fifo:uuid()) ->
                     ok | not_found |
                     {'error','no_servers'}.
vm_stop(VM) when is_binary(VM) ->
    vm_stop(VM, []).

%%--------------------------------------------------------------------
%% @doc Stops a VM on the hypervisor and allows passing options like
%%   forced stop.
%% @end
%%--------------------------------------------------------------------
-spec vm_stop(VM::fifo:uuid(),
              Options::[atom() | {atom(), term()}]) ->
                     ok | not_found |
                     {'error','no_servers'}.
vm_stop(VM, []) when
      is_binary(VM)->
    send({vm, stop, VM});

vm_stop(VM, [force]) when
      is_binary(VM)->
    send({vm, stop, force, VM}).

%%--------------------------------------------------------------------
%% @doc Restarts a VM on the hypervisor
%% @end
%%--------------------------------------------------------------------
-spec vm_reboot(VM::fifo:uuid()) ->
                       ok | not_found |
                       {'error','no_servers'}.
vm_reboot(VM) when
      is_binary(VM) ->
    vm_reboot(VM, []).

%%--------------------------------------------------------------------
%% @doc Restarts a VM on the hypervisor and allows passing options like
%%   forced restart.
%% @end
%%--------------------------------------------------------------------
-spec vm_reboot(VM::fifo:uuid(),
                Options::[atom() | {atom(), term()}]) ->
                       ok | not_found |
                       {'error','no_servers'}.
vm_reboot(VM, []) when
      is_binary(VM) ->
    send({vm, reboot, VM});

vm_reboot(VM, [force]) when
      is_binary(VM) ->
    send({vm, reboot, force, VM}).

%%--------------------------------------------------------------------
%% @doc Triggers a delete, this does not remove the VM from the
%%   database instead just pokes the hypervisor which, when the delete
%%   was successful calls a unregister.
%% @end
%%--------------------------------------------------------------------
-spec vm_delete(VM::fifo:uuid()) ->
                       ok | not_found |
                       {'error','no_servers'}.
vm_delete(VM) when
      is_binary(VM) ->
    send({vm, delete, VM}).

%%--------------------------------------------------------------------
%% @doc Updates a VM by attempting to resize it from package
%%   perspective and changing some of the config values.
%% @end
%%--------------------------------------------------------------------
-spec vm_update(VM::fifo:uuid(),
                Package::binary() | undefined,
                Config::fifo:config_list()) -> ok | not_found |
                              {'error','no_servers'}.
vm_update(VM, Package, Config) when
      is_binary(VM),
      is_list(Config) ->
    send({vm, update, VM, Package, Config}).

%%--------------------------------------------------------------------
%% @doc Sets a attribute on the VM object in the database - this does
%%   not change the VM on the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec vm_set(VM::fifo:uuid(),
             Attribute::fifo:keys(),
             Value::fifo:value() | delete) ->
                    ok | not_found |
                    {'error','no_servers'}.
vm_set(VM, Attribute, Value) when
      is_binary(VM) ->
    send({vm, set, VM, Attribute, Value}).


%%--------------------------------------------------------------------
%% @doc Sets some attributes on the VM object in the database - this
%%    does not change the VM on the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec vm_set(VM::fifo:uuid(),
             Attributes::fifo:config_list()) -> ok | not_found |
                                                {'error','no_servers'}.
vm_set(VM, Attributes) when
      is_binary(VM) ->
    send({vm, set, VM, [{K, V} || {K, V} <- Attributes,
                                             is_binary(K)]}).

%%--------------------------------------------------------------------
%% @doc Adds a log to the VM that will be timestamped on the server.
%% @end
%%--------------------------------------------------------------------
-spec vm_log(Vm::fifo:uuid(), Log::binary()) -> ok |
                                                {'error','no_servers'}.
vm_log(Vm, Log) ->
    send({vm, log, Vm, Log}).

%%--------------------------------------------------------------------
%% @doc Creates a ZFS new snapshot of a given VM.
%% @end
%%--------------------------------------------------------------------
-spec vm_snapshot(Vm::fifo:uuid(), Comment::binary()) ->
                         {ok, fifo:uuid()} |
                         {'error','no_servers'}.
vm_snapshot(Vm, Comment) ->
    send({vm, snapshot, Vm, Comment}).

%%--------------------------------------------------------------------
%% @doc Deletes a ZFS snapshot from the VM.
%% @end
%%--------------------------------------------------------------------
-spec vm_delete_snapshot(Vm::fifo:uuid(),
                         UUID::binary()) ->
                                ok |
                                {'error','no_servers'}.
vm_delete_snapshot(Vm, UUID) ->
    send({vm, snapshot, delete, Vm, UUID}).

%%--------------------------------------------------------------------
%% @doc Rolls back a snapshot of a VM, this will <b>delete</b> all
%%   snapshots between the current state and the rolled back snapshot!
%% @end
%%--------------------------------------------------------------------
-spec vm_rollback_snapshot(Vm::fifo:uuid(),
                           UUID::binary()) ->
                                  ok |
                                  {'error','no_servers'}.
vm_rollback_snapshot(Vm, UUID) ->
    send({vm, snapshot, rollback, Vm, UUID}).

%%--------------------------------------------------------------------
%% @doc Lists the UUID's of all VM's known to the server.
%% @end
%%--------------------------------------------------------------------
-spec vm_list() -> {ok, [fifo:uuid()]} |
                   {'error','no_servers'}.
vm_list() ->
    send({vm, list}).

%%--------------------------------------------------------------------
%% @doc Lists the UUID's of all VM's known to the server filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec vm_list(Reqs::[fifo:matcher()]) ->
                     {ok, [fifo:uuid()]} |
                     {'error','no_servers'}.
vm_list(Reqs) ->
    send({vm, list, Reqs}).

%%%===================================================================
%%% Hypervisor Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Registeres a hypervisor with sniffle, passing it's ID
%%   (usually the hostname) the ip and the Port of the chunter server
%%   running on it.
%% @end
%%--------------------------------------------------------------------
-spec hypervisor_register(Hypervisor::binary(),
                          Host::inet:ip_address() | inet:hostname(),
                          Port::inet:port_number()) ->
                                 ok |
                                 {'error','no_servers'}.
hypervisor_register(Hypervisor, Host, Port) when
      is_binary(Hypervisor),
      is_integer(Port),
      Port > 0 ->
    send({hypervisor, register, Hypervisor, Host, Port}).

%%--------------------------------------------------------------------
%% @doc Removes a hypervisor form sniffle.
%% @end
%%--------------------------------------------------------------------
-spec hypervisor_unregister(Hypervisor::binary()) ->
                                   ok | not_found |
                                   {'error','no_servers'}.
hypervisor_unregister(Hypervisor) ->
    send({hypervisor, unregister, Hypervisor}).

%%--------------------------------------------------------------------
%% @doc Reads the hypervisor object from the database.
%% @end
%%--------------------------------------------------------------------
-spec hypervisor_get(Hypervisor::binary()) ->
                            not_found |
                            {ok, fifo:hypervisor()} |
                            {'error','no_servers'}.
hypervisor_get(Hypervisor) ->
    send({hypervisor, get, Hypervisor}).

%%--------------------------------------------------------------------
%% @doc Sets a attribute of the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec hypervisor_set(Hypervisor::binary(),
                     Resource::fifo:keys(),
                     Value::fifo:value() | delete) ->
                            ok | not_found |
                            {'error','no_servers'}.
hypervisor_set(Hypervisor, Resource, Value) ->
    send({hypervisor, set, Hypervisor, Resource, Value}).

%%--------------------------------------------------------------------
%% @doc Sets multiple attributes of the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec hypervisor_set(Hypervisor::binary(),
                     Resources::fifo:config_list()) ->
                            ok | not_found |
                            {'error','no_servers'}.
hypervisor_set(Hypervisor, Resources) ->
    send({hypervisor, set, Hypervisor, Resources}).

%%--------------------------------------------------------------------
%% @doc Lists all hypervisors known to the system.
%% @end
%%--------------------------------------------------------------------
-spec hypervisor_list() -> {ok, [binary()]} |
                           {'error','no_servers'}.
hypervisor_list() ->
    send({hypervisor, list}).

%%--------------------------------------------------------------------
%% @doc Lists all hypervisors known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec hypervisor_list(Requirements::[fifo:matcher()]) ->
                             {ok, [fifo:hypervisor()]} |
                             {'error','no_servers'}.
hypervisor_list(Requirements) ->
    send({hypervisor, list, Requirements}).

%%%===================================================================
%%%  DATASET Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new dataset with the given UUID.
%% @end
%%--------------------------------------------------------------------
-spec dataset_create(Dataset::binary()) ->
                            ok | duplicate |
                            {'error','no_servers'}.
dataset_create(Dataset) ->
    send({dataset, create, Dataset}).

%%--------------------------------------------------------------------
%% @doc Tries to import a dataset from a dsapi server URL.
%% @end
%%--------------------------------------------------------------------
-spec dataset_import(URL::binary()) ->
                            {ok, UUID::fifo:uuid()} |
                            {error, Reason::term()} |
                            {'error','no_servers'}.
dataset_import(URL) ->
    send({dataset, import, URL}).

%%--------------------------------------------------------------------
%% @doc Deletes a dataset from the server, this will also delete the
%%   image from the database.
%% @end
%%--------------------------------------------------------------------
-spec dataset_delete(Dataset::binary()) ->
                            ok | not_found |
                            {'error','no_servers'}.
dataset_delete(Dataset) ->
    send({dataset, delete, Dataset}).

%%--------------------------------------------------------------------
%% @doc Gets a dataset from the database.
%% @end
%%--------------------------------------------------------------------
-spec dataset_get(Dataset::binary()) ->
                         {'error','no_servers'} |
                         not_found |
                         {ok, [{Key::term(), Key::term()}]}.
dataset_get(Dataset) ->
    send({dataset, get, Dataset}).
%%--------------------------------------------------------------------
%% @doc Sets a attribute of a dataset.
%% @end
%%--------------------------------------------------------------------
-spec dataset_set(Dataset::fifo:dataset_id(),
                  Attribute::fifo:keys(),
                  Value::fifo:value() | delete) ->
                         ok | not_found |
                         {'error','no_servers'}.
dataset_set(Dataset, Attribute, Value) ->
    send({dataset, set, Dataset, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets multiple attributes of a dataset.
%% @end
%%--------------------------------------------------------------------
-spec dataset_set(Dataset::fifo:dataset_id(),
                  Attirbutes::fifo:attr_list()) ->
                         ok |
                         not_found |
                         {'error','no_servers'}.
dataset_set(Dataset, Attributes) ->
    send({dataset, set, Dataset, Attributes}).

%%--------------------------------------------------------------------
%% @doc Lists all datasets known to the system.
%% @end
%%--------------------------------------------------------------------
-spec dataset_list() ->
                          {ok, Datasets::[binary()]} |
                          {'error','no_servers'}.
dataset_list() ->
    send({dataset, list}).

%%--------------------------------------------------------------------
%% @doc Lists all datasets known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec dataset_list(Reqs::term()) ->
                          {ok, Datasets::[binary()]} |
                          {'error','no_servers'}.
dataset_list(Reqs) ->
    send({dataset, list, Reqs}).

%%%===================================================================
%%%  IMG Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new image part on the server.
%% @end
%%--------------------------------------------------------------------
-spec img_create(Img::binary(), Idx::pos_integer(), Data::binary()) ->
                        ok |
                        {'error','no_servers'}.
img_create(Img, Idx, Data) ->
    send({img, create, Img, Idx, Data}).

%%--------------------------------------------------------------------
%% @doc Deletes an entire image form the server
%% @end
%%--------------------------------------------------------------------
-spec img_delete(Img::binary()) ->
                        ok | not_found |
                        {'error','no_servers'}.
img_delete(Img) ->
    send({img, delete, Img}).

%%--------------------------------------------------------------------
%% @doc Deletes a image part from the server
%% @end
%%--------------------------------------------------------------------
-spec img_delete(Img::binary(), Idx::pos_integer()) ->
                        ok | not_found |
                        {'error','no_servers'}.
img_delete(Img, Idx) ->
    send({img, delete, Img, Idx}).

%%--------------------------------------------------------------------
%% @doc Reads a image part from the server
%% @end
%%--------------------------------------------------------------------
-spec img_get(Img::binary(), Idx::pos_integer()) ->
                     {'error','no_servers'} |
                     not_found |
                     {ok, binary()}.
img_get(Img, Idx) ->
    send({img, get, Img, Idx}).

%%--------------------------------------------------------------------
%% @doc Lists all images on the server.
%% @end
%%--------------------------------------------------------------------
-spec img_list() ->
                      {ok, Imgs::[fifo:uuid()]} |
                      {'error','no_servers'}.
img_list() ->
    send({img, list}).

%%--------------------------------------------------------------------
%% @doc Lists all parts for a images on the server.
%% @end
%%--------------------------------------------------------------------
-spec img_list(Img::binary()) ->
                      {ok, Parts::[pos_integer()]} |
                      {'error','no_servers'}.
img_list(Img) ->
    send({img, list, Img}).

%%%===================================================================
%%%  PACKAGE Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new package and returns it's UUID.
%% @end
%%--------------------------------------------------------------------
-spec package_create(Name::binary()) ->
                            {ok, UUID::fifo:uuid()} |
                            duplicate |
                            {'error','no_servers'}.
package_create(Name) when
      is_binary(Name) ->
    send({package, create, Name}).

%%--------------------------------------------------------------------
%% @doc Deletes a package from the database
%% @end
%%--------------------------------------------------------------------
-spec package_delete(Package::fifo:uuid()) ->
                            ok | not_found |
                            {'error','no_servers'}.
package_delete(Package) when
      is_binary(Package) ->
    send({package, delete, Package}).

%%--------------------------------------------------------------------
%% @doc Reads a package from the database
%% @end
%%--------------------------------------------------------------------
-spec package_get(Package::binary()) ->
                         not_found |
                         {ok, Package::fifo:config_list()} |
                         {'error','no_servers'}.
package_get(Package) when
      is_binary(Package) ->
    send({package, get, Package}).

%%--------------------------------------------------------------------
%% @doc Sets a attribute on the pacakge.
%% @end
%%--------------------------------------------------------------------
-spec package_set(Package::fifo:package_id(),
                  Attribute::fifo:keys(),
                  Value::fifo:value() | delete) -> ok | not_found |
                                          {'error','no_servers'}.
package_set(Package, Attribute, Value)  when
      is_binary(Package) ->
    send({package, set, Package, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets multiple attributes on the pacakge.
%% @end
%%--------------------------------------------------------------------
-spec package_set(Package::fifo:package_id(),
                  Attirbutes::fifo:config_list()) ->
                         ok | not_found |
                         {'error','no_servers'}.
package_set(Package, Attributes) when
      is_binary(Package),
      is_list(Attributes) ->
    send({package, set, Package, Attributes}).

%%--------------------------------------------------------------------
%% @doc Lists all packages known to the system.
%% @end
%%--------------------------------------------------------------------
-spec package_list() ->
                          {ok, Packages::[binary()]} |
                          {'error','no_servers'}.
package_list() ->
    send({package, list}).

%%--------------------------------------------------------------------
%% @doc Lists all packages known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec package_list(Reqs::[fifo:matcher()]) -> {ok, Packages::[binary()]} |
                                              {'error','no_servers'}.
package_list(Reqs) ->
    send({package, list, Reqs}).

%%%===================================================================
%%%  Iprange Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new IP range on the server. All ips are passed as
%%   integer or binary.
%% @end
%%--------------------------------------------------------------------
-spec iprange_create(Iprange::binary(),
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

%%--------------------------------------------------------------------
%% @doc Deletes a iprange from the server.
%% @end
%%--------------------------------------------------------------------
-spec iprange_delete(Iprange::binary()) ->
                            ok | not_found |
                            {'error','no_servers'}.

iprange_delete(Iprange) ->
    send({iprange, delete, Iprange}).

%%--------------------------------------------------------------------
%% @doc Reads all the info of a iprange from the server.
%% @end
%%--------------------------------------------------------------------
-spec iprange_get(Iprange::binary()) ->
                         not_found |
                         {ok, fifo:config_list()} |
                         {'error','no_servers'}.

iprange_get(Iprange) ->
    send({iprange, get, Iprange}).

%%--------------------------------------------------------------------
%% @doc Sets a attribute on the iprange.
%% @end
%%--------------------------------------------------------------------
-spec iprange_set(Iprange::fifo:iprange_id(),
                  Attribute::fifo:keys(),
                  Value::fifo:value() | delete) ->
                         ok | not_found |
                         {'error','no_servers'}.
iprange_set(Iprange, Attribute, Value)  when
      is_binary(Iprange) ->
    send({iprange, set, Iprange, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets multiple attributes on the iprange.
%% @end
%%--------------------------------------------------------------------
-spec iprange_set(Iprange::fifo:iprange_id(),
                  Attirbutes::fifo:config_list()) ->
                         ok | not_found |
                         {'error','no_servers'}.
iprange_set(Iprange, Attributes) when
      is_binary(Iprange),
      is_list(Attributes) ->
    send({iprange, set, Iprange, Attributes}).

%%--------------------------------------------------------------------
%% @doc Tries to release a claimed ip address from a range.
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc Claims a ipaddress and returns the address and the relevant
%%   network information.
%% @end
%%--------------------------------------------------------------------
-spec iprange_claim(Iprange::binary()) ->
                           not_found |
                           {ok, {Tag::binary(),
                                 IP::pos_integer(),
                                 Netmask::pos_integer(),
                                 Gateway::pos_integer()}} |
                           {error, failed} |
                           {'error','no_servers'}.
iprange_claim(Iprange) ->
    send({iprange, claim, Iprange}).

%%--------------------------------------------------------------------
%% @doc Lists all ip ranges known to the system.
%% @end
%%--------------------------------------------------------------------
-spec iprange_list() ->
                          {ok, [binary()]} |
                          {'error','no_servers'}.
iprange_list() ->
    send({iprange, list}).

%%--------------------------------------------------------------------
%% @doc Lists all ip ranges known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec iprange_list(Reqs::[fifo:matcher()]) ->
                          {ok, [binary()]} |
                          {'error','no_servers'}.
iprange_list(Reqs) ->
    send({iprange, list, Reqs}).

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

-spec send(MSG::fifo:sniffle_message()) ->
                  ok |
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
