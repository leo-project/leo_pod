# **leo_pod** -  A Fast Erlang worker pool manager

## Usage

```Erlang

    %% Prepare
    PodName = 'first_leo_pod',
    PodSize     = 8,
    MaxOverflow = 16,
    ModName     = 'leo_pod_mod',
    WorkerArgs  = [{protocol, tcp},
                   {host, "127.0.0.1"},
                   {port, 8080}],
    leo_pod:child_spec(PodName, PodSize, MaxOverflow, ModName, WorkerArgs),

    %% Execute - [checkout > call > checkin]
    {ok, Worker} = leo_pod:checkout(PodName),
    {ok, _Reply} = gen_server:call(Worker, {Fun, <<"Hello Hal,">>}),
    ok = leo_pod:checkin(PodName, Worker),
    ok.

```

## License

leo_pod's license is "Apache License Version 2.0"
