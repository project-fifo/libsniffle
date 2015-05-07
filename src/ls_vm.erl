-module(ls_vm).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         get/1,
         dry_run/3, create/3, delete/1, delete/2, store/1, store/2,
         register/2, unregister/1,
         state/2,
         creating/2,
         update/3, update/4,
         add_nic/2, remove_nic/2, primary_nic/2,
         set_config/2,
         set_info/2,
         set_service/2,
         set_backup/2,
         set_snapshot/2,
         set_metadata/2,
         log/2,
         snapshot/2, delete_snapshot/2, rollback_snapshot/2,
         commit_snapshot_rollback/2, promote_snapshot/3,
         incremental_backup/4, full_backup/3,
         restore_backup/2, restore_backup/3, restore_backup/4, delete_backup/3,
         service_enable/2, service_disable/2, service_clear/2,
         service_refresh/2, service_restart/2,
         add_fw_rule/2, remove_fw_rule/2,
         list/0, list/2,
         start/1, stop/1, stop/2,
         reboot/1, reboot/2,
         owner/2, owner/3
        ]).

-define(UUID, <<UUID:36/binary>>).

%%%===================================================================
%%% VM Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds a new firewall rule to a VM.
%% @end
%%--------------------------------------------------------------------
-spec add_fw_rule(VM::fifo:vm_id(), Hypervisor::fifo:fw_rule()) ->
                         ok |
                         {'error','no_servers'}.
add_fw_rule(UUID, Rule) ->
    send({vm, fw, add, UUID, Rule}).

%%--------------------------------------------------------------------
%% @doc Deletes a firewall rule from a VM.
%% @end
%%--------------------------------------------------------------------
-spec remove_fw_rule(VM::fifo:vm_id(), Hypervisor::fifo:fw_rule()) ->
                            ok |
                            {'error','no_servers'}.
remove_fw_rule(UUID, Rule) ->
    send({vm, fw, remove, UUID, Rule}).

%%--------------------------------------------------------------------
%% @doc Initiates the creating a new VM, this will just return the
%%   UUID progress is directy written to the object in the database.
%% @end
%%--------------------------------------------------------------------
-spec create(PackageID::fifo:package_id(), DatasetID::fifo:package_id(),
             Config::fifo:attr_list()) ->
                    {error, no_servers} |
                    {ok, UUID::binary()}.

create(PackageID, DatasetID, Config) ->
    send({vm, create, PackageID, DatasetID, Config}).

%%--------------------------------------------------------------------
%% @doc Initiates the creating a new VM, this will just return the
%%   UUID progress is directy written to the object in the database.
%% @end
%%--------------------------------------------------------------------
-spec dry_run(PackageID::binary(), DatasetID::binary(), Config::[{Key::binary(), Value::term()}]) ->
                     {error, no_servers} |
                     {ok, success|failed}.

dry_run(PackageID, DatasetID, Config) ->
    send({vm, dry_run, PackageID, DatasetID, Config}).

%%--------------------------------------------------------------------
%% @doc Registeres an existing VM with sniffle.
%% @end
%%--------------------------------------------------------------------
-spec register(VM::fifo:vm_id(), Hypervisor::fifo:hypervisor_id()) ->
                      ok |
                      {'error','no_servers'}.
register(VM, Hypervisor) when
      is_binary(VM),
      is_binary(Hypervisor) ->
    send({vm, register, VM, Hypervisor}).

%%--------------------------------------------------------------------
%% @doc Unregisteres a VM from the database, this does not include
%%   deleting it from the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec unregister(VM::fifo:vm_id()) ->
                        ok |
                        not_found |
                        {'error','no_servers'}.
unregister(VM) when
      is_binary(VM) ->
    send({vm, unregister, VM}).

%%--------------------------------------------------------------------
%% @doc Reads the VM attribute from the database.
%% @end
%%--------------------------------------------------------------------
-spec get(VM::fifo:vm_id()) ->
                 not_found |
                 {ok, fifo:vm()} |
                 {'error','no_servers'}.
get(VM) when
      is_binary(VM) ->
    send({vm, get, VM}).

%%--------------------------------------------------------------------
%% @doc Starts a VM on the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec start(VM::fifo:vm_id()) ->
                   ok | not_found |
                   {'error','no_servers'}.
start(VM) when
      is_binary(VM) ->
    send({vm, start, VM}).

%%--------------------------------------------------------------------
%% @doc Stops a VM on the hypervisor.
%% @end
%%--------------------------------------------------------------------
-spec stop(VM::fifo:vm_id()) ->
                  ok | not_found |
                  {'error','no_servers'}.
stop(VM) when is_binary(VM) ->
    stop(VM, []).

%%--------------------------------------------------------------------
%% @doc Stops a VM on the hypervisor and allows passing options like
%%   forced stop.
%% @end
%%--------------------------------------------------------------------
-spec stop(VM::fifo:vm_id(),
           Options::[atom() | {atom(), term()}]) ->
                  ok | not_found |
                  {'error','no_servers'}.
stop(VM, []) when
      is_binary(VM)->
    send({vm, stop, VM});

stop(VM, [force]) when
      is_binary(VM)->
    send({vm, stop, force, VM}).

%%--------------------------------------------------------------------
%% @doc Restarts a VM on the hypervisor
%% @end
%%--------------------------------------------------------------------
-spec reboot(VM::fifo:vm_id()) ->
                    ok | not_found |
                    {'error','no_servers'}.
reboot(VM) when
      is_binary(VM) ->
    reboot(VM, []).

%%--------------------------------------------------------------------
%% @doc Restarts a VM on the hypervisor and allows passing options like
%%   forced restart.
%% @end
%%--------------------------------------------------------------------
-spec reboot(VM::fifo:vm_id(),
             Options::[atom() | {atom(), term()}]) ->
                    ok | not_found |
                    {'error','no_servers'}.
reboot(VM, []) when
      is_binary(VM) ->
    send({vm, reboot, VM});

reboot(VM, [force]) when
      is_binary(VM) ->
    send({vm, reboot, force, VM}).

%%--------------------------------------------------------------------
%% @doc Triggers a delete, this does not remove the VM from the
%%   database instead just pokes the hypervisor which, when the delete
%%   was successful calls a unregister.
%% @end
%%--------------------------------------------------------------------
-spec delete(User::fifo:user_id() | undefined,
             VM::fifo:vm_id()) ->
                    ok | not_found |
                    {'error','no_servers'}.

delete(User, VM) when
      is_binary(VM) ->
    send({vm, delete, User, VM}).

delete(VM) ->
    delete(undefined, VM).

-spec store(User::fifo:user_id() | undefined,
            VM::fifo:vm_id()) ->
                   ok | not_found |
                   {'error','no_servers'}.
store(User, VM) when
      is_binary(VM) ->
    send({vm, store, User, VM}).

store(VM) ->
    store(undefined, VM).

%%--------------------------------------------------------------------
%% @doc Changes the owner of a VM.
%% @end
%%--------------------------------------------------------------------
-spec owner(User::fifo:user_id() | undefined,
            VM::fifo:vm_id(), Owner::fifo:uuid()) ->
                   ok | not_found |
                   {'error','no_servers'}.
owner(User, VM, Owner) ->
    send({vm, owner, User, VM, Owner}).

owner(VM, Owner) ->
    owner(undefined, VM, Owner).

%%--------------------------------------------------------------------
%% @doc Updates a VM by attempting to resize it from package
%%   perspective and changing some of the config values.
%% @end
%%--------------------------------------------------------------------
-spec update(User::fifo:user_id() | undefined, VM::fifo:vm_id(),
             Package::binary() | undefined,
             Config::fifo:attr_list()) -> ok | not_found |
                                          {'error','no_servers'}.
update(User, VM, Package, Config) when
      is_binary(VM),
      is_list(Config) ->
    send({vm, update, User, VM, Package, Config}).

update(VM, Package, Config) ->
    update(undefined, VM, Package, Config).

%%--------------------------------------------------------------------
%% @doc Adds a new interface to a VM.
%% @end
%%--------------------------------------------------------------------
-spec add_nic(VM::fifo:vm_id(),
              Network::fifo:network_id()) -> ok | not_found |
                                             {'error','no_servers'|
                                              'update_failed'|
                                              'claim_failed'|
                                              'not_stopped'|_Reason}.
add_nic(VM, IPRange) when
      is_binary(VM),
      is_binary(IPRange) ->
    send({vm, nic, add, VM, IPRange}).

%%--------------------------------------------------------------------
%% @doc Remove a interface from a VM.
%% @end
%%--------------------------------------------------------------------
-spec remove_nic(VM::fifo:vm_id(),
                 Mac::binary()) -> ok | not_found |
                                   {'error','no_servers'|
                                    'update_failed'|
                                    'not_found'|
                                    'not_stopped'|_Reason}.
remove_nic(VM, Mac) when
      is_binary(VM),
      is_binary(Mac) ->
    send({vm, nic, remove, VM, Mac}).

%%--------------------------------------------------------------------
%% @doc Sets a NIC as primary interface.
%% @end
%%--------------------------------------------------------------------
-spec primary_nic(VM::fifo:vm_id(),
                  Mac::binary()) -> ok | not_found |
                                    {'error','no_servers'|
                                     'update_failed'|
                                     'not_found'|
                                     'not_stopped'|_Reason}.
primary_nic(VM, Mac) when
      is_binary(VM),
      is_binary(Mac) ->
    send({vm, nic, primary, VM, Mac}).

%%--------------------------------------------------------------------
%% @doc Sets the state of a VM.
%% @end
%%--------------------------------------------------------------------
-spec state(VM::fifo:vm_id(),
            State::binary()) ->
                   ok | not_found |
                   {'error','no_servers'}.
state(VM, State) when
      is_binary(VM) ->
    send({vm, state, VM, State}).

%%--------------------------------------------------------------------
%% @doc Sets the state of a VM.
%% @end
%%--------------------------------------------------------------------
-spec creating(VM::fifo:vm_id(),
               Creating::false | {atom(), tuple()}) ->
                      ok | not_found |
                      {'error','no_servers'}.
creating(VM, Creating) when Creating == false,
                         is_tuple(Creating) ->
    send({vm, creating, VM, Creating}).

%%--------------------------------------------------------------------
%% @doc Sets a service attribute on the VM object in the database
%% @end
%%--------------------------------------------------------------------
-spec set_service(VM::fifo:vm_id(),
                  [{Attribute::fifo:keys(),
                    Value::fifo:value() | delete}]) ->
                         ok | not_found |
                         {'error','no_servers'}.
set_service(VM, AVs) when
      is_binary(VM) ->
    send({vm, set_service, VM, AVs}).

%%--------------------------------------------------------------------
%% @doc Sets a backup attribute on the VM object in the database
%% @end
%%--------------------------------------------------------------------
-spec set_backup(VM::fifo:vm_id(),
                 [{Attribute::fifo:keys(),
                   Value::fifo:value() | delete}]) ->
                        ok | not_found |
                        {'error','no_servers'}.
set_backup(VM, AVs) when
      is_binary(VM) ->
    send({vm, set_backup, VM, AVs}).

%%--------------------------------------------------------------------
%% @doc Sets a backup attribute on the VM object in the database
%% @end
%%--------------------------------------------------------------------
-spec set_snapshot(VM::fifo:vm_id(),
                   [{Attribute::fifo:keys(),
                     Value::fifo:value() | delete}]) ->
                          ok | not_found |
                          {'error','no_servers'}.
set_snapshot(VM, AVs) when
      is_binary(VM) ->
    send({vm, set_snapshot, VM, AVs}).

%%--------------------------------------------------------------------
%% @doc Sets a metadata attribute on the VM object in the database
%% @end
%%--------------------------------------------------------------------
-spec set_metadata(VM::fifo:vm_id(),
                   [{Attribute::fifo:keys(),
                     Value::fifo:value() | delete}]) ->
                          ok | not_found |
                          {'error','no_servers'}.
set_metadata(VM, AVs) when
      is_binary(VM) ->
    send({vm, set_metadata, VM, AVs}).

%%--------------------------------------------------------------------
%% @doc Sets a config attribute on the VM object in the database
%% @end
%%--------------------------------------------------------------------
-spec set_config(VM::fifo:vm_id(),
                 [{Attribute::fifo:keys(),
                   Value::fifo:value() | delete}]) ->
                        ok | not_found |
                        {'error','no_servers'}.
set_config(VM, AVs) when
      is_binary(VM) ->
    send({vm, set_config, VM, AVs}).

%%--------------------------------------------------------------------
%% @doc Sets a info attribute on the VM object in the database
%% @end
%%--------------------------------------------------------------------
-spec set_info(VM::fifo:vm_id(),
               [{Attribute::fifo:keys(),
                 Value::fifo:value() | delete}]) ->
                      ok | not_found |
                      {'error','no_servers'}.
set_info(VM, AVs) when
      is_binary(VM) ->
    send({vm, set_info, VM, AVs}).

%%--------------------------------------------------------------------
%% @doc Adds a log to the VM that will be timestamped on the server.
%% @end
%%--------------------------------------------------------------------
-spec log(Vm::fifo:uuid(), Log::binary()) -> ok |
                                             {'error','no_servers'}.
log(Vm, Log) ->
    send({vm, log, Vm, Log}).

%%--------------------------------------------------------------------
%% @doc Creates a full backup of a VM.
%% @end
%%--------------------------------------------------------------------
incremental_backup(Vm, Parent, Comment, Opts) ->
    send({vm, backup, incremental, Vm, Parent, Comment, Opts}).

%%--------------------------------------------------------------------
%% @doc Creates a full backup of a VM.
%% @end
%%--------------------------------------------------------------------
full_backup(Vm, Comment, Opts) ->
    send({vm, backup, full, Vm, Comment, Opts}).

%%--------------------------------------------------------------------
%% @doc Restores a VM from a backup.
%% @end
%%--------------------------------------------------------------------
restore_backup(Vm, Backup) ->
    send({vm, backup, restore, Vm, Backup}).

%%--------------------------------------------------------------------
%% @doc Restores a VM from a backup to a specific hypervisor.
%% @end
%%--------------------------------------------------------------------
restore_backup(Vm, Backup, Hypervisor) ->
    restore_backup(undefined, Vm, Backup, Hypervisor).

restore_backup(User, Vm, Backup, Hypervisor) ->
    send({vm, backup, restore, User, Vm, Backup, Hypervisor}).

%%--------------------------------------------------------------------
%% @doc Deletes the backup of a VM.
%% @end
%%--------------------------------------------------------------------
delete_backup(Vm, Backup, Where) ->
    send({vm, backup, delete, Vm, Backup, Where}).

%%--------------------------------------------------------------------
%% @doc Enables a service on a VM.
%% @end
%%--------------------------------------------------------------------
service_enable(Vm, Service) ->
    send({vm, service, enable, Vm, Service}).

%%--------------------------------------------------------------------
%% @doc Enables a service on a VM.
%% @end
%%--------------------------------------------------------------------
service_disable(Vm, Service) ->
    send({vm, service, disable, Vm, Service}).

%%--------------------------------------------------------------------
%% @doc Enables a service on a VM.
%% @end
%%--------------------------------------------------------------------
service_clear(Vm, Service) ->
    send({vm, service, clear, Vm, Service}).

%%--------------------------------------------------------------------
%% @doc Restarts a service on a VM.
%% @end
%%--------------------------------------------------------------------
service_restart(Vm, Service) ->
    send({vm, service, restart, Vm, Service}).

%%--------------------------------------------------------------------
%% @doc Refreshes a service on a VM.
%% @end
%%--------------------------------------------------------------------
service_refresh(Vm, Service) ->
    send({vm, service, refresh, Vm, Service}).

%%--------------------------------------------------------------------
%% @doc Creates a ZFS new snapshot of a given VM.
%% @end
%%--------------------------------------------------------------------
-spec snapshot(Vm::fifo:uuid(), Comment::binary()) ->
                      {ok, fifo:uuid()} |
                      {'error','no_servers'}.
snapshot(Vm, Comment) ->
    send({vm, snapshot, Vm, Comment}).

%%--------------------------------------------------------------------
%% @doc Deletes a ZFS snapshot from the VM.
%% @end
%%--------------------------------------------------------------------
-spec delete_snapshot(Vm::fifo:uuid(),
                      UUID::binary()) ->
                             ok |
                             {'error','no_servers'}.
delete_snapshot(Vm, UUID) ->
    send({vm, snapshot, delete, Vm, UUID}).

%%--------------------------------------------------------------------
%% @doc Rolls back a snapshot of a VM, this will <b>delete</b> all
%%   snapshots between the current state and the rolled back snapshot!
%% @end
%%--------------------------------------------------------------------
-spec rollback_snapshot(Vm::fifo:uuid(),
                        UUID::fifo:uuid()) ->
                               ok |
                               {'error','no_servers'}.
rollback_snapshot(Vm, UUID) ->
    send({vm, snapshot, rollback, Vm, UUID}).

%%--------------------------------------------------------------------
%% @doc Confirms the rollback of a snapshot, this will delete all
%%   snapshots between the current state and the rolled back snapshot!
%% @end
%%--------------------------------------------------------------------
-spec commit_snapshot_rollback(Vm::fifo:uuid(),
                               UUID::fifo:uuid()) ->
                                      ok |
                                      {'error','no_servers'}.
commit_snapshot_rollback(Vm, UUID) ->
    send({vm, snapshot, commit_rollback, Vm, UUID}).

%%--------------------------------------------------------------------
%% @doc Rolls back a snapshot of a VM, this will <b>delete</b> all
%%   snapshots between the current state and the rolled back snapshot!
%% @end
%%--------------------------------------------------------------------
-spec promote_snapshot(Vm::fifo:uuid(),
                       UUID::fifo:uuid(),
                       Config::fifo:object()) ->
                              {ok, UUID::fifo:dataset_id()} |
                              {'error','no_servers'}.

promote_snapshot(Vm, UUID, Config) ->
    send({vm, snapshot, promote, Vm, UUID, Config}).
%%--------------------------------------------------------------------
%% @doc Lists the UUID's of all VM's known to the server.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [fifo:uuid()]} |
                {'error','no_servers'}.
list() ->
    send({vm, list}).

%%--------------------------------------------------------------------
%% @doc Lists the UUID's of all VM's known to the server filtered by
%%   given matchers.
%% @end
%%--------------------------------------------------------------------
-spec list(Reqs::[fifo:matcher()], boolean()) ->
                  {ok, [{Ranking::integer(), fifo:vm_id()}]} |
                  {ok, [{Ranking::integer(), fifo:vm()}]} |
                  {'error','no_servers'}.
list(Reqs, Full) ->
    send({vm, list, Reqs, Full}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec send(MSG::fifo:sniffle_vm_message()) ->
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
