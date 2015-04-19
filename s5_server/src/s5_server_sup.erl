-module(s5_server_sup).
-behaviour(supervisor).
-include("s5_records.hrl").
-export([start_link/1, start_child/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link(LSock) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
  supervisor:start_child(?SERVER, []).

init([LSock]) ->
  Server = {s5_server_handler, {s5_server_handler, start_link, [LSock]},
    temporary, brutal_kill, worker, [s5_server_handler]},
  Children = [Server],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
