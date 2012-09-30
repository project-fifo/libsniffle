-module(libsniffle).

-export([
	 start/0, 
	 servers/0,
	 register_on_connect/1,
	 register_on_disconnect/1
	]).

-export([
	 vm_register/2,
	 vm_unregister/1,
	 vm_attribute_get/2,
	 vm_attributes_get/1,
	 vm_attribute_set/3,
	 vm_attributes_set/2,
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
    application:start(libsnarl).

-spec servers() -> [any()].
servers() ->
    libsnarl_server:servers().

register_on_connect(Fn) ->
    libsnarl_server:register_on_connect(Fn).

register_on_disconnect(Fn) ->
    libsnarl_server:register_on_disconnect(Fn).

%%%===================================================================
%%% VM Functions
%%%===================================================================


-spec vm_register(VM::binary(), Hypervisor::binary()) -> ok | error.
vm_register(VM, Hypervisor) ->
    send({vm, register, VM, Hypervisor}).


-spec vm_unregister(VM::binary()) -> ok | error.
vm_unregister(VM) ->
    send({vm, unregister, VM}).

-spec vm_attribute_get(VM::binary(), Attribute::binary()) -> any().
vm_attribute_get(VM, Attribute) ->
    send({vm, attribute, get, VM, Attribute}).

-spec vm_attribute_set(VM::binary(), Attribute::binary(), Value::any()) -> any().
vm_attribute_set(VM, Attribute, Value) ->
    send({vm, attribute, set, VM, Attribute, Value}).

-spec vm_attributes_get(VM::binary()) -> [{binary(), any()}].
vm_attributes_get(VM) ->
    send({vm, attributes, get, VM}).

-spec vm_attributes_set(VM::binary(), [{Attribute::binary(), Value::any()}]) -> any().
vm_attributes_set(VM, Attributes) ->
    send({vm, attributes, set, VM, Attributes}).

-spec vm_list() -> [vm()].
vm_list() ->
    send({vm, list}).

-spec vm_list(User::binary()) -> [vm()].
vm_list(User) ->
    send({vm, list, User}).


%%%===================================================================
%%% Hypervisor Functions
%%%===================================================================


-spec hypervisor_register(Hypervisor::binary(), Host::binary(), Port::integer()) -> ok | error.
hypervisor_register(Hypervisor, Host, Port) ->
    send({hypervisor, register, Hypervisor, Host, Port}).

-spec hypervisor_unregister(Hypervisor::binary()) -> ok | error.
hypervisor_unregister(Hypervisor) ->
    send({hyperisor, unregister, Hypervisor}).

-spec hypervisor_resource_get(Hypervisor::binary(), Resource::binary()) -> any().
hypervisor_resource_get(Hypervisor, Resource) ->
    send({hyperisor, resource, get, Hypervisor, Resource}).

-spec hypervisor_resource_set(Hypervisor::binary(), Resource::binary(), Value::any()) -> ok| error.
hypervisor_resource_set(Hypervisor, Resource, Value) ->
    send({hyperisor, resource, set, Hypervisor, Resource, Value}).

-spec hypervisor_list() -> [hypervisor()].
hypervisor_list() ->
    send({hyperisor, list}).

-spec hypervisor_list(User::binary()) -> [hypervisor()].
hypervisor_list(User) ->
    send({hyperisor, list, User}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

send(Msg) ->
    libsnarl_server:send(Msg).
