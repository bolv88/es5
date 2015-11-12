-module(s5_client_sup).
-behaviour(supervisor).
-include("s5_records.hrl").
-export([start_link/2, start_child/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link(LSock, #server_conf{} = ServerConf) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock, ServerConf]).

start_child() ->
  supervisor:start_child(?SERVER, []).

init([LSock, ServerConf]) ->
  %Server = {s5_client_handler, {s5_client_handler, start_link, [LSock, ServerConf]},
    %Setemporary, brutal_kill, worker, [s5_client_handler]},
  Server = {s5_client_handler, {s5_client_handler, start_link, [LSock, ServerConf]},
    temporary, infinity, worker, [s5_client_handler]},
  Children = [Server],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
