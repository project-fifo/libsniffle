-module(libsniffle).

-export([
	 start/0, 
	 servers/0
	]).

-export([
	 vm_register/2,
	 vm_unregister/1,
	 vm_attribute_get/1,
	 vm_attribute_get/2,
	 vm_attribute_set/2,
	 vm_attribute_set/3,
	 vm_list/0,
	 vm_list/1
	]).

-export([
	 hypervisor_register/3,
	 hypervisor_unregister/1,
	 hypervisor_resource_get/1,
	 hypervisor_resource_get/2,
	 hypervisor_resource_set/2,
	 hypervisor_resource_set/3,
	 hypervisor_list/0,
	 hypervisor_list/1
	]).

-export([
	 dataset_create/1,
	 dataset_delete/1,
	 dataset_get/1,
	 dataset_attribute_get/1,
	 dataset_attribute_get/2,
	 dataset_attribute_set/2,
	 dataset_attribute_set/3,
	 dataset_list/0,
	 dataset_list/1
	 ]).

-export([
	 package_create/1,
	 package_delete/1,
	 package_get/1,
	 package_attribute_get/1,
	 package_attribute_get/2,
	 package_attribute_set/2,
	 package_attribute_set/3,
	 package_list/0,
	 package_list/1
	 ]).

-export([
	 iprange_create/7,
	 iprange_delete/1,
	 iprange_get/1,
	 iprange_release/2,
	 iprange_claim/1,
	 iprange_list/0,
	 iprange_list/1
	]).

-type vm() :: term().
-type hypervisor() :: term().
-type dataset() :: term().
-type package() :: term().

%%%===================================================================
%%% Generatl Functions
%%%===================================================================

-spec start() -> ok | error.
start() ->
    application:start(mdns_client_lib),
    application:start(libsniffle).

-spec servers() -> [any()].
servers() ->
    libsniffle_server:servers().

%%%===================================================================
%%% VM Functions
%%%===================================================================


-spec vm_register(VM::binary(), Hypervisor::binary()) -> ok | error.
vm_register(VM, Hypervisor) ->
    send({vm, register, ensure_binary(VM), ensure_binary(Hypervisor)}).

-spec vm_unregister(VM::binary()) -> ok | error.
vm_unregister(VM) ->
    send({vm, unregister, ensure_binary(VM)}).

-spec vm_attribute_get(VM::binary(), Attribute::binary()) -> any().
vm_attribute_get(VM, Attribute) ->
    send({vm, attribute, get, ensure_binary(VM), ensure_binary(Attribute)}).

-spec vm_attribute_set(VM::binary(), Attribute::binary(), Value::any()) -> any().
vm_attribute_set(VM, Attribute, Value) ->
    send({vm, attribute, set, ensure_binary(VM), ensure_binary(Attribute), Value}).

-spec vm_attribute_get(VM::binary()) -> [{binary(), any()}].
vm_attribute_get(VM) ->
    send({vm, attribute, get, ensure_binary(VM)}).

-spec vm_attribute_set(VM::binary(), [{Attribute::binary(), Value::any()}]) -> any().
vm_attribute_set(VM, Attributes) ->
    send({vm, attribute, set, ensure_binary(VM), [{ensure_binary(K), V} || {K, V} <- Attributes]}).

-spec vm_list() -> [vm()].
vm_list() ->
    send({vm, list}).

-spec vm_list(User::binary()) -> [vm()].
vm_list(User) ->
    send({vm, list, ensure_binary(User)}).

%%%===================================================================
%%% Hypervisor Functions
%%%===================================================================

-spec hypervisor_register(Hypervisor::binary(), Host::binary(), Port::integer()) -> ok | error.
hypervisor_register(Hypervisor, Host, Port) ->
    send({hypervisor, register, ensure_binary(Hypervisor), Host, Port}).

-spec hypervisor_unregister(Hypervisor::binary()) -> ok | error.
hypervisor_unregister(Hypervisor) ->
    send({hypervisor, unregister, ensure_binary(Hypervisor)}).

-spec hypervisor_resource_get(Hypervisor::binary(), Resource::binary()) -> any().
hypervisor_resource_get(Hypervisor, Resource) ->
    send({hypervisor, resource, get, ensure_binary(Hypervisor), ensure_binary(Resource)}).

-spec hypervisor_resource_get(Hypervisor::binary()) -> any().
hypervisor_resource_get(Hypervisor) ->
    send({hypervisor, resource, get, ensure_binary(Hypervisor)}).

-spec hypervisor_resource_set(Hypervisor::binary(), Resource::binary(), Value::any()) -> ok| error.
hypervisor_resource_set(Hypervisor, Resource, Value) ->
    send({hypervisor, resource, set, ensure_binary(Hypervisor), ensure_binary(Resource), Value}).

-spec hypervisor_resource_set(Hypervisor::binary(), Resources::[term()]) -> ok | error.
hypervisor_resource_set(Hypervisor, Resources) ->
    send({hypervisor, resource, set, ensure_binary(Hypervisor), Resources}).

-spec hypervisor_list() -> [hypervisor()].
hypervisor_list() ->
    send({hypervisor, list}).

-spec hypervisor_list(Requirements::[term()]) -> [hypervisor()].
hypervisor_list(Requirements) ->
    send({hypervisor, list, ensure_binary(Requirements)}).

%%%===================================================================
%%%  DATASET Functions
%%%===================================================================

-spec dataset_create(Dataset::binary()) -> ok.
dataset_create(Dataset) ->
    send({dataset, create, Dataset}).

-spec dataset_delete(Dataset::binary()) -> ok.
dataset_delete(Dataset) ->
    send({dataset, delete, Dataset}).

-spec dataset_get(Dataset::binary()) -> Dataset::dataset().
dataset_get(Dataset) ->
    send({dataset, get, Dataset}).

-spec dataset_attribute_get(Dataset::binary()) -> {ok, [{Key::term(), Key::term()}]}.
dataset_attribute_get(Dataset) ->
    send({dataset, attribute, get, Dataset}).

-spec dataset_attribute_get(Dataset::binary(), Attribute::term()) -> {ok, term()}.
dataset_attribute_get(Dataset, Attribute) ->
    send({dataset, attribute, get, Dataset, Attribute}).

-spec dataset_attribute_set(Dataset::binary(), Attirbutes::[{Key::term(), Value::term()}]) -> ok.
dataset_attribute_set(Dataset, Attributes) ->
    send({dataset, attribute, set, Dataset, Attributes}).

-spec dataset_attribute_set(Dataset::binary(), Attribute::term(), Value::term()) -> ok.
dataset_attribute_set(Dataset, Attribute, Value) ->
    send({dataset, attribute, set, Dataset, Attribute, Value}).


-spec dataset_list() -> Datasets::[dataset()].
dataset_list() ->
    send({dataset, list}).

-spec dataset_list(User::binary()) -> Datasets::[dataset()].
dataset_list(User) ->
    send({dataset, list, User}).

%%%===================================================================
%%%  PACKAGE Functions
%%%===================================================================

-spec package_create(Package::binary()) -> ok.
package_create(Package) ->
    send({Package, create, Package}).

-spec package_delete(Package::binary()) -> ok.
package_delete(Package) ->
    send({package, delete, Package}).

-spec package_get(Package::binary()) -> Package::package().
package_get(Package) ->
    send({package, delete, Package}).

-spec package_attribute_get(Package::binary()) -> {ok, [{Key::term(), Key::term()}]}.
package_attribute_get(Package) ->
    send({package, attribute, get, Package}).

-spec package_attribute_get(Package::binary(), Attribute::term()) -> {ok, term()}.
package_attribute_get(Package, Attribute) ->
    send({package, attribute, get, Package, Attribute}).

-spec package_attribute_set(Package::binary(), Attirbutes::[{Key::term(), Value::term()}]) -> ok.
package_attribute_set(Package, Attributes) ->
    send({package, attribute, set, Package, Attributes}).

-spec package_attribute_set(Package::binary(), Attribute::term(), Value::term()) -> ok.
package_attribute_set(Package, Attribute, Value) ->
    send({package, attribute, set, Package, Attribute, Value}).

-spec package_list() -> Package::[package()].
package_list() ->
    send({package, list}).

-spec package_list(User::binary()) -> Packages::[package()].
package_list(User) ->
    send({package, list, User}).

%%%===================================================================
%%%  PACKAGE Functions
%%%===================================================================

iprange_create(Iprange, Network, Gateway, Netmask, First, Last, Tag) ->
    send({iprange, create, Iprange, Network, Gateway, Netmask, First, Last, Tag}).

iprange_delete(Iprange) ->
    send({iprange, delete, Iprange}).


iprange_get(Iprange) ->
    send({iprange, get, Iprange}).

iprange_release(Iprange, Ip) ->
    send({iprange, release, Iprange, Ip}).

iprange_claim(Iprange) ->
    send({iprange, claim, Iprange}).

iprange_list() ->
    send({iprange, list}).

iprange_list(User) ->
    send({iprange, list, User}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
send(Msg) ->
    libsniffle_server:send(Msg).

-spec ensure_binary(any()) -> binary().
ensure_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
ensure_binary(L) when is_list(L) ->
    list_to_binary(L);
ensure_binary(B) when is_binary(B) ->
    B;
ensure_binary(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I));
ensure_binary(F) when is_float(F) ->
    list_to_binary(float_to_list(F));
ensure_binary(T) ->
    term_to_binary(T).
