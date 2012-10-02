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
	 hypervisor_resource_get/2,
	 hypervisor_resource_set/3,
	 hypervisor_list/0,
	 hypervisor_list/1
	]).

-type vm() :: any().
-type hypervisor() :: any().

%%%===================================================================
%%% Generatl Functions
%%%===================================================================

-spec start() -> ok | error.
start() ->
    application:start(zmq_mdns_client),
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
    send({hyperisor, unregister, ensure_binary(Hypervisor)}).

-spec hypervisor_resource_get(Hypervisor::binary(), Resource::binary()) -> any().
hypervisor_resource_get(Hypervisor, Resource) ->
    send({hyperisor, resource, get, ensure_binary(Hypervisor), ensure_binary(Resource)}).

-spec hypervisor_resource_set(Hypervisor::binary(), Resource::binary(), Value::any()) -> ok| error.
hypervisor_resource_set(Hypervisor, Resource, Value) ->
    send({hyperisor, resource, set, ensure_binary(Hypervisor), ensure_binary(Resource), Value}).

-spec hypervisor_list() -> [hypervisor()].
hypervisor_list() ->
    send({hyperisor, list}).

-spec hypervisor_list(User::binary()) -> [hypervisor()].
hypervisor_list(User) ->
    send({hyperisor, list, ensure_binary(User)}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

send(Msg) ->
    libsniffle_server:send(Msg).

ensure_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
ensure_binary(L) when is_list(L) ->
    list_to_binary(L);
ensure_binary(B) when is_binary(B)->
    B;
ensure_binary(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I));
ensure_binary(F) when is_float(F) ->
    list_to_binary(float_to_list(F));
ensure_binary(T) ->
    term_to_binary(T).
