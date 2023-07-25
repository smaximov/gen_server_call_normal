-module(gen_server_call_normal_server).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_info/2, handle_cast/2]).

start_link(Arg) ->
  gen_server:start_link(?MODULE, Arg, []).

init({Iteration, Timeout, Puts}) ->
  erlang:send_after(Timeout, self(), shutdown),
  {ok, {Iteration, Puts}}.

handle_call(do_work, _From, State) ->
  {reply, ok, State}.

handle_info(shutdown, {Iteration, Puts} = State) ->
  if
    Puts -> io:format("~p: exiting...~n", [Iteration]);
    true -> ok
  end,
  {stop, normal, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.