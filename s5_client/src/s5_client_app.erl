-module(s5_client_app).
-include("s5_records.hrl").
-behaviour(application).
-export([start_out/0, start/2, stop/1]).

start_out() ->
  application:start(s5_client).

start(_Type, _Args) ->
  ServerConf = #server_conf{host="blublu.me", port=2345}, %=tochange, to config file
  %Opts = [],
  ListenPort = 6002, %=tochange, change to env config file
  {ok, LSock} = gen_tcp:listen(ListenPort, [binary, {packet, 0}, {reuseaddr, true}, {active, true}, {backlog, 128}]),
  case s5_client_sup:start_link(LSock, ServerConf) of
    {ok, Pid} ->
      s5_client_sup:start_child(),
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_S) ->
  ok.


