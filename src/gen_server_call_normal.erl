-module(gen_server_call_normal).
-behaviour(supervisor).
-export([run/3, init/1]).

run(Iters, Timeout, Puts) ->
  {ok, Sup} = supervisor:start_link(?MODULE, []),

  try
    loop(#{}, Iters, Sup, Timeout, Puts)
  after
    gen_server:stop(Sup)
  end.

init(_InitArg) ->
  SupFlags = #{strategy => simple_one_for_one},
  ChildSpecs = [#{id => gen_server_call_normal_server,
                  start => {gen_server_call_normal_server, start_link, []},
                  restart => transient}],
  {ok, {SupFlags, ChildSpecs}}.

loop(Acc, 0, _Sup, _Timeout, _Puts) ->
  Acc;

loop(Acc, Iters, Sup, Timeout, Puts) ->
  {ok, Server} = supervisor:start_child(Sup, [{Iters, Timeout, Puts}]),

  receive
  after
    Timeout -> ok
  end,

  try
    gen_server:call(Server, do_work),
    Acc1 = maps:update_with(success, fun(X) -> X + 1 end, 1, Acc),
    loop(Acc1, Iters - 1, Sup, Timeout, Puts)
  catch
    exit:{Reason, _Request} ->
      Acc2 = maps:update_with(Reason, fun(X) -> X + 1 end, 1, Acc),
      loop(Acc2, Iters - 1, Sup, Timeout, Puts)
  end.
