-module(libsniffle).

-export([
	 start/0,
	 servers/0,
	 create/3
	]).

-export([
	 vm_register/2,
	 vm_unregister/1,
	 vm_attribute_get/1,
	 vm_attribute_get/2,
	 vm_attribute_set/2,
	 vm_attribute_set/3,
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

%%%===================================================================
%%% Generatl Functions
%%%===================================================================

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

-spec vm_attribute_get(VM::fifo:uuid(),
		       Attribute::binary()) ->
			      not_found |
			      {ok, fifo:value()} |
			      {'error','no_servers'}.
vm_attribute_get(VM, Attribute) when
      is_binary(VM),
      is_binary(Attribute) ->
    send({vm, attribute, get, VM, Attribute}).

-spec vm_attribute_set(VM::fifo:uuid(),
		       Attribute::binary(),
		       Value::any()) -> ok | not_found |
					{'error','no_servers'}.
vm_attribute_set(VM, Attribute, Value) when
      is_binary(VM),
      is_binary(Attribute) ->
    send({vm, attribute, set, VM, Attribute, Value}).

-spec vm_attribute_get(VM::fifo:uuid()) ->
			      not_found |
			      {ok, fifo:config_list()} |
			      {'error','no_servers'}.
vm_attribute_get(VM) when
      is_binary(VM) ->
    send({vm, attribute, get, VM}).

-spec vm_attribute_set(VM::fifo:uuid(),
		       Attributes::fifo:config_list()) -> ok | not_found |
							  {'error','no_servers'}.
vm_attribute_set(VM, Attributes) when
      is_binary(VM) ->
    send({vm, attribute, set, VM, [{K, V} || {K, V} <- Attributes,
					     is_binary(K)]}).


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

-spec hypervisor_resource_get(Hypervisor::binary(), Resource::binary()) ->
				     not_found |
				     {ok, fifo:value()} |
				     {'error','no_servers'}.
hypervisor_resource_get(Hypervisor, Resource) ->
    send({hypervisor, resource, get, Hypervisor, Resource}).

-spec hypervisor_resource_get(Hypervisor::binary()) ->
				     not_found |
				     {ok, [{binary(), fifo:value()}]} |
				     {'error','no_servers'}.

hypervisor_resource_get(Hypervisor) ->
    send({hypervisor, resource, get, Hypervisor}).

-spec hypervisor_resource_set(Hypervisor::binary(), Resource::binary(), Value::fifo:value()) ->
				     ok |
				     not_found |
				     {'error','no_servers'}.
hypervisor_resource_set(Hypervisor, Resource, Value) ->
    send({hypervisor, resource, set, Hypervisor, Resource, Value}).

-spec hypervisor_resource_set(Hypervisor::binary(), Resources::fifo:config_list()) ->
				     ok | not_found |
				     {'error','no_servers'}.
hypervisor_resource_set(Hypervisor, Resources) ->
    send({hypervisor, resource, set, Hypervisor, Resources}).

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

-spec dataset_attribute_get(Dataset::binary()) -> {'error','no_servers'} |
						  not_found |
						  {ok, [{Key::term(), Key::term()}]}.
dataset_attribute_get(Dataset) ->
    send({dataset, attribute, get, Dataset}).

-spec dataset_attribute_get(Dataset::binary(), Attribute::term()) ->
				   {'error','no_servers'} |
				   not_found |
				   {ok, term()}.
dataset_attribute_get(Dataset, Attribute) ->
    send({dataset, attribute, get, Dataset, Attribute}).

-spec dataset_attribute_set(Dataset::binary(), Attirbutes::[{Key::term(), Value::term()}]) ->
				   ok |
				   not_found |
				   {'error','no_servers'}.
dataset_attribute_set(Dataset, Attributes) ->
    send({dataset, attribute, set, Dataset, Attributes}).

-spec dataset_attribute_set(Dataset::binary(), Attribute::term(), Value::term()) ->
				   ok | not_found |
				   {'error','no_servers'}.
dataset_attribute_set(Dataset, Attribute, Value) ->
    send({dataset, attribute, set, Dataset, Attribute, Value}).


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

-spec package_attribute_get(Package::binary()) ->
				   not_found |
				   {ok, fifo:config_list()} |
				   {'error','no_servers'}.
package_attribute_get(Package) when
      is_binary(Package) ->
    send({package, attribute, get, Package}).

-spec package_attribute_get(Package::binary(),
			    Attribute::binary()) ->
				   not_found |
				   {ok, fifo:value()} |
				   {'error','no_servers'}.
package_attribute_get(Package, Attribute) when
      is_binary(Package),
      is_binary(Attribute) ->
    send({package, attribute, get, Package, Attribute}).

-spec package_attribute_set(Package::binary(),
			    Attirbutes::fifo:config_list()) -> ok | not_found |
							       {'error','no_servers'}.
package_attribute_set(Package, Attributes) when
      is_binary(Package),
      is_list(Attributes) ->
    send({package, attribute, set, Package, Attributes}).

-spec package_attribute_set(Package::binary(),
			    Attribute::binary(),
			    Value::fifo:value()) -> ok | not_found |
						    {'error','no_servers'}.
package_attribute_set(Package, Attribute, Value)  when
      is_binary(Package),
      is_binary(Attribute) ->
    send({package, attribute, set, Package, Attribute, Value}).

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
		     Tag::binary()) ->
			    ok | doublicate |
			    {'error','no_servers'}.


iprange_create(Iprange, Network, Gateway, Netmask, First, Last, Tag) when
      is_binary(Iprange),
      is_binary(Tag),
      is_integer(Network),
      is_integer(Gateway), Network =:= (Gateway band Netmask),
      is_integer(Netmask),
      is_integer(First), Network =:= (First band Netmask),
      is_integer(Last), Network =:= (Last band Netmask) ->
    send({iprange, create, Iprange,
	  Network, Gateway, Netmask,
	  First, Last,
	  Tag});

iprange_create(Iprange, Network, Gateway, Netmask, First, Last, Tag) when
      is_binary(Iprange),
      is_binary(Tag) ->
    iprange_create(Iprange,
		   ip_to_int(Network),
		   ip_to_int(Gateway),
		   ip_to_int(Netmask),
		   ip_to_int(First),
		   ip_to_int(Last),
		   Tag).

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
