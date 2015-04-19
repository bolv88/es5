-module(s5_server_app).
-include("s5_records.hrl").
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
  %Opts = [],
  ListenPort = 2345, %=tochange, change to env config file
  {ok, LSock} = gen_tcp:listen(ListenPort, [binary, {packet, 0}, {reuseaddr, true}, {active, true}, {backlog, 128}]),
  case s5_server_sup:start_link(LSock) of
    {ok, Pid} ->
      s5_server_sup:start_child(),
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_S) ->
  ok.


