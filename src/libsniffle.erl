-module(libsniffle).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         start/0,
         servers/0,
         version/0
        ]).

-export([
         grouping_add/2,
         grouping_delete/1,
         grouping_get/1,
         grouping_metadata_set/2,
         grouping_metadata_set/3,
         grouping_list/2,
         grouping_list/1,
         grouping_list/0,
         grouping_add_element/2,
         grouping_remove_element/2,
         grouping_add_grouping/2,
         grouping_remove_grouping/2
        ]).

-export([
         dtrace_add/2,
         dtrace_delete/1,
         dtrace_get/1,
         dtrace_set/2,
         dtrace_set/3,
         dtrace_list/2,
         dtrace_list/1,
         dtrace_list/0,
         dtrace_run/2
        ]).

-export([
         dataset_create/1,
         dataset_import/1,
         dataset_delete/1,
         dataset_get/1,
         dataset_set/2,
         dataset_set/3,
         dataset_list/0,
         dataset_list/1,
         dataset_list/2
        ]).

-export([
         img_create/4,
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
         package_list/1,
         package_list/2
        ]).

-export([
         network_create/1,
         network_create/2,
         network_delete/1,
         network_get/1,
         network_add_iprange/2,
         network_add_iprange/3,
         network_remove_iprange/2,
         network_set/2,
         network_set/3,
         network_list/2,
         network_list/3
        ]).

-export([
         iprange_create/8,
         iprange_delete/1,
         iprange_get/1,
         iprange_release/2,
         iprange_claim/1,
         iprange_list/2,
         iprange_list/3,
         iprange_set/2,
         iprange_set/3
        ]).

-export([ip_to_bin/1,
         ip_to_int/1]).

-export([cloud_status/0]).

-define(UUID, <<UUID:36/binary>>).

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
%%% Grouping Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds a new grouping to sniffle, the name is a plain
%%   binary, the type is the atom cluster or stack
%%   encapsulated in $ signs.
%%   A UUID is returned.
%% @end
%%--------------------------------------------------------------------
-spec grouping_add(Name::binary(),
                   Type::atom()) ->
                          {ok, UUID::fifo:uuid()} |
                          duplicate |
                          {'error','no_servers'}.
grouping_add(Name, cluster) when
      is_binary(Name)->
    send({grouping, add, Name, cluster});
grouping_add(Name, none) when
      is_binary(Name)->
    send({grouping, add, Name, none});
grouping_add(Name, stack) when
      is_binary(Name)->
    send({grouping, add, Name, stack}).

%%--------------------------------------------------------------------
%% @doc Deletes a grouping script from the library
%% @end
%%--------------------------------------------------------------------
-spec grouping_delete(UUID::fifo:uuid()) ->
                           ok |
                           {'error','no_servers'}.
grouping_delete(ID) when
      is_binary(ID)->
    send({grouping, delete, ID}).

%%--------------------------------------------------------------------
%% @doc Reads a grouping script and returns the jsx object for it.
%% @end
%%--------------------------------------------------------------------
-spec grouping_get(UUID::fifo:uuid()) ->
                        {ok, Data::fifo:config_list()} |
                        not_found |
                        {'error','no_servers'}.
grouping_get(ID) when
      is_binary(ID)->
    send({grouping, get, ID}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database.
%% @end
%%--------------------------------------------------------------------
-spec grouping_list() ->
                         {ok, [UUID::fifo:uuid()]} |
                         {'error','no_servers'}.
grouping_list()->
    send({grouping, list}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec grouping_list([Requirement::fifo:matcher()]) ->
                         {ok, [{Ranking::integer(),
                                ID::fifo:grouping_id()}]} |
                         {'error','no_servers'}.
grouping_list(Requirements)->
    send({grouping, list, Requirements}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec grouping_list([Requirement::fifo:matcher()], boolean()) ->
                         {ok, [{Ranking::integer(),
                                ID::fifo:grouping_id()|fifo:object()}]} |
                         {'error','no_servers'}.
grouping_list(Requirements, Full)->
    send({grouping, list, Requirements, Full}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec grouping_metadata_set(Grouping::fifo:uuid(),
                 Attribute::fifo:keys(),
                 Value::fifo:value() | delete) ->
                        ok | not_found |
                        {'error','no_servers'}.
grouping_metadata_set(Grouping, Attribute, Value) when
      is_binary(Grouping) ->
    send({grouping, metadata, set, Grouping, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets options on a dtace script. The root key 'config' has a
%%   special meaning here since it holds replacement variables.
%% @end
%%--------------------------------------------------------------------
-spec grouping_metadata_set(Grouping::fifo:uuid(),
                 Attributes::fifo:config_list()) ->
                        ok | not_found |
                        {'error','no_servers'}.
grouping_metadata_set(Grouping, Attributes) when
      is_binary(Grouping) ->
    send({grouping, metadata, set, Grouping, Attributes}).


grouping_add_element(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, element, add, Grouping, Element}).

grouping_remove_element(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, element, remove, Grouping, Element}).

grouping_add_grouping(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, grouping, add, Grouping, Element}).

grouping_remove_grouping(Grouping, Element)
  when is_binary(Grouping), is_binary(Element) ->
    send({grouping, grouping, remove, Grouping, Element}).
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
                         {ok, [{Ranking::integer(),
                                ID::fifo:dtrace_id()}]} |
                         {'error','no_servers'}.
dtrace_list(Requirements)->
    send({dtrace, list, Requirements}).

%%--------------------------------------------------------------------
%% @doc Lists the ID's of all scripts in the database filtered by
%%   the passed requirements.
%% @end
%%--------------------------------------------------------------------
-spec dtrace_list([Requirement::fifo:matcher()], boolean()) ->
                         {ok, [{Ranking::integer(),
                                ID::fifo:dtrace_id()|fifo:object()}]} |
                         {'error','no_servers'}.
dtrace_list(Requirements, Full)->
    send({dtrace, list, Requirements, Full}).

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
                          {ok, Datasets::[{Ranking::integer(),
                                           ID::fifo:dataset_id()}]} |
                          {'error','no_servers'}.
dataset_list(Reqs) ->
    send({dataset, list, Reqs}).

%%--------------------------------------------------------------------
%% @doc Lists all datasets known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec dataset_list(Reqs::term(), boolean()) ->
                          {ok, Datasets::[{Ranking::integer(),
                                           ID::fifo:dataset_id()|fifo:object()}]} |
                          {'error','no_servers'}.
dataset_list(Reqs, Full) ->
    send({dataset, list, Reqs, Full}).

%%%===================================================================
%%%  IMG Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new image part on the server.
%% @end
%%--------------------------------------------------------------------
-spec img_create(Img::fifo:dataset_id(), Idx::integer()|done,
                 Data::binary(), Ref::term()) ->
                        {ok, Ref1::term()} |
                        {'error','no_servers'}.
img_create(?UUID, Idx, Data, Ref) when Idx >= 0,
                                  is_binary(Data) ->
    send({img, create, UUID, Idx, Data, Ref}).

%%--------------------------------------------------------------------
%% @doc Deletes an entire image form the server
%% @end
%%--------------------------------------------------------------------
-spec img_delete(Img::fifo:dataset_id()) ->
                        ok | not_found |
                        {'error','no_servers'}.
img_delete(?UUID) ->
    send({img, delete, UUID}).

%%--------------------------------------------------------------------
%% @doc Deletes a image part from the server
%% @end
%%--------------------------------------------------------------------
-spec img_delete(Img::fifo:dataset_id(), Idx::integer()) ->
                        ok | not_found |
                        {'error','no_servers'}.
img_delete(?UUID, Idx) when Idx >= 0 ->
    send({img, delete, UUID, Idx}).

%%--------------------------------------------------------------------
%% @doc Reads a image part from the server
%% @end
%%--------------------------------------------------------------------
-spec img_get(Img::fifo:dataset_id(), Idx::integer()) ->
                     {'error','no_servers'} |
                     not_found |
                     {ok, binary()}.
img_get(?UUID, Idx) when Idx >= 0 ->
    send({img, get, UUID, Idx}).

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
-spec img_list(Img::fifo:dataset_id()) ->
                      {ok, Parts::[pos_integer()]} |
                      {'error','no_servers'}.
img_list(?UUID) ->
    send({img, list, ?UUID}).

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
-spec package_list(Reqs::[fifo:matcher()]) ->
                          {ok, [{Ranking::integer(),
                                 ID::fifo:package_id()}]} |
                          {'error','no_servers'}.
package_list(Reqs) ->
    send({package, list, Reqs}).

%%--------------------------------------------------------------------
%% @doc Lists all packages known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec package_list(Reqs::[fifo:matcher()], boolean()) ->
                          {ok, [{Ranking::integer(),
                                 ID::fifo:package_id()|fifo:object()}]} |
                          {'error','no_servers'}.
package_list(Reqs, Full) ->
    send({package, list, Reqs, Full}).


%%%===================================================================
%%%  NETWORK Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new network and returns it's UUID.
%% @end
%%--------------------------------------------------------------------
-spec network_create(Name::binary()) ->
                            {ok, UUID::fifo:uuid()} |
                            duplicate |
                            {'error','no_servers'}.
network_create(Name) ->
    network_create(mdns, Name).

network_create(Sniffle, Name) when
      is_binary(Name) ->
    send(Sniffle, {network, create, Name}).

%%--------------------------------------------------------------------
%% @doc Deletes a network from the database
%% @end
%%--------------------------------------------------------------------
-spec network_delete(Network::fifo:uuid()) ->
                            ok | not_found |
                            {'error','no_servers'}.
network_delete(Network) when
      is_binary(Network) ->
    send({network, delete, Network}).

%%--------------------------------------------------------------------
%% @doc Reads a network from the database
%% @end
%%--------------------------------------------------------------------
-spec network_get(Network::binary()) ->
                         not_found |
                         {ok, Network::fifo:config_list()} |
                         {'error','no_servers'}.
network_get(Network) when
      is_binary(Network) ->
    send({network, get, Network}).

%%--------------------------------------------------------------------
%% @doc Adds a iprange to a network
%% @end
%%--------------------------------------------------------------------
-spec network_add_iprange(Network::binary(), IPrange::binary()) ->
                                 not_found |
                                 ok |
                                 {'error','no_servers'}.
network_add_iprange(Network, IPRange) ->
      network_add_iprange(mdns, Network, IPRange).

network_add_iprange(Sniffle, Network, IPRange) when
      is_binary(Network),
      is_binary(IPRange) ->
    send(Sniffle, {network, add_iprange, Network, IPRange}).

%%--------------------------------------------------------------------
%% @doc Adds a iprange to a network
%% @end
%%--------------------------------------------------------------------
-spec network_remove_iprange(Network::binary(), IPrange::binary()) ->
                                    not_found |
                                    ok |
                                    {'error','no_servers'}.
network_remove_iprange(Network, IPRange) when
      is_binary(Network),
      is_binary(IPRange) ->
    send({network, remove_iprange, Network, IPRange}).

%%--------------------------------------------------------------------
%% @doc Sets a attribute on the pacakge.
%% @end
%%--------------------------------------------------------------------
-spec network_set(Network::fifo:network_id(),
                  Attribute::fifo:keys(),
                  Value::fifo:value() | delete) -> ok | not_found |
                                                   {'error','no_servers'}.
network_set(Network, Attribute, Value)  when
      is_binary(Network) ->
    send({network, set, Network, Attribute, Value}).

%%--------------------------------------------------------------------
%% @doc Sets multiple attributes on the pacakge.
%% @end
%%--------------------------------------------------------------------
-spec network_set(Network::fifo:network_id(),
                  Attirbutes::fifo:config_list()) ->
                         ok | not_found |
                         {'error','no_servers'}.
network_set(Network, Attributes) when
      is_binary(Network),
      is_list(Attributes) ->
    send({network, set, Network, Attributes}).

%%--------------------------------------------------------------------
%% @doc Lists all networks known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec network_list(Reqs::[fifo:matcher()], boolean()) ->
                          {ok, [{Ranking::integer(),
                                 ID::fifo:network_id()|fifo:object()}]} |
                          {'error','no_servers'}.
network_list(Reqs, Full) ->
    network_list(mdns, Reqs, Full).

network_list(Sniffle, Reqs, Full) ->
    send(Sniffle, {network, list, Reqs, Full}).

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
%% @doc Lists all ip ranges known to the system filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec iprange_list(Reqs::[fifo:matcher()], boolean()) ->
                          {ok, [{Ranking::integer(),
                                 ID::fifo:iprange_id()|fifo:object()}]} |
                          {'error','no_servers'}.
iprange_list(Reqs, Full) ->
    iprange_list(mdns, Reqs, Full).

iprange_list(Sniffle, Reqs, Full) ->
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
