# leo_pod -  A Fast Erlang worker pool manager

**leo_pod** is an Erlang worker pool manager.
It is implemented no to use ETS (Erlang Term Storage).
This is because we met a problem of ETS when we run it on high spec machines.

## Usage

See files under the [`test/`](https://github.com/leo-project/leo_pod/tree/develop/test) directory in the repository.

```Erlang

    %% Prepare
    PodName = 'first_leo_pod',
    PodSize     = 8,
    MaxOverflow = 16,
    ModName     = 'leo_pod_mod',
    WorkerArgs  = [{protocol, tcp},
                   {host, "127.0.0.1"},
                   {port, 8080}],
    leo_pod:start_link(PodName, PodSize, MaxOverflow, ModName, WorkerArgs),

    %% Execute - [checkout > call > checkin]
    {ok, Worker} = leo_pod:checkout(PodName),
    {ok, _Reply} = gen_server:call(Worker, {echo, <<"Hello Hal,">>}),
    ok = leo_pod:checkin(PodName, Worker),
    ok.

```

## Usage in Leo Project

**leo_pod** is used in [**leo_rpc**](https://github.com/leo-project/leo_rpc) library.
It is used to mangage the number of process of RPC clients for each node.
It is necessary because some OS has the limit for the number of file descriptors
and therefore connections to the other nodes.

## License

leo_pod's license is "Apache License Version 2.0"
