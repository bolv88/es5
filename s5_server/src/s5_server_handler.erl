-module(s5_server_handler).
-behavior(gen_fsm).
%-export([start_link/1, t1/2, t2/2]).
%-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4, 
%    terminate/3, code_change/4]). 
-compile(export_all).
-record(state, {lsock, client_socket, server_socket, workstat=0}).

start_link(LSock)->
  yb_common:print_info("start link", []),
  gen_fsm:start_link(?MODULE, [LSock], []).

init([LSock]) ->
  yb_common:print_info("init ~p", [LSock]),
  {ok, version_identifier, #state{lsock=LSock, workstat=0}, 0}.

version_identifier(timeout, State = #state{lsock=LSock}) ->
  yb_common:print_info("accepting sock connect", []),
  {ok, ClientSocket} = gen_tcp:accept(LSock),
  %{ok, {IP, _Port}} = inet:peername(ClientSocket),
  s5_server_sup:start_child(),
  yb_common:print_info("receive connect", []),
  {next_state, version_identifier, State#state{client_socket=ClientSocket, workstat=1}, 30000};
  
version_identifier({tcp, _Socket, <<_Ver, _Nmethods, _Methods/binary>>}, State) ->
  yb_common:print_info("version_identifier ~p", [<<_Ver, _Nmethods, _Methods/binary>>]),
  gen_tcp:send(State#state.client_socket, yb_common:bin_encode(<<5, 0>>)),
  %{next_state, validate_user, State}.
  {next_state, loop_trans, State}.

validate_user({tcp, _Socket,  <<Data/binary>>}, State) ->
  yb_common:print_info("validate_user ~p", [Data]),
  {next_state, loop_trans, State}.

loop_trans({tcp, _Socket, <<Data/binary>>}, State) ->
  yb_common:print_info("loop_trans ~p", [Data]),
  {next_state, loop_trans, State};

loop_trans({tcp_closed, ClientSocket}, State = #state{client_socket = ClientSocket, server_socket = ServerSocket}) ->
  %gen_tcp:close(ServerSocket),
  {stop, "client close", State};

loop_trans({tcp_closed, ServerSocket}, State = #state{client_socket = ClientSocket, server_socket = ServerSocket}) ->
  gen_tcp:close(ClientSocket),
  {stop, "server close", State}.

handle_info({tcp , Socket, Data}, StateName, State) ->
  yb_common:print_info("tcp info ~p at ~p", [Data, StateName]),
  ?MODULE:StateName({tcp, Socket, Data}, State);
  %{next_state, StateName, State}.

handle_info({tcp_closed , Socket}, StateName, State) ->
  yb_common:print_info("tcp info tcp_closed at ~p", [StateName]),
  ?MODULE:StateName({tcp_closed, Socket}, State);

handle_info(timeout, StateName, #state{lsock=LSock, workstat = 0} = State) ->
  yb_common:print_info("timout 0", []),
  {ok, ClientSocket} = gen_tcp:accept(LSock),
  s5_server_sup:start_child(),
  {next_state, StateName, State#state{client_socket = ClientSocket}};

handle_info(timeout, StateName, #state{client_socket = ClientSocket, server_socket = ServerSocket, workstat = 1} = State) ->
  yb_common:print_info("timout 1", []),
  gen_tcp:close(ServerSocket),
  gen_tcp:close(ClientSocket),
  {stop, "time out", StateName, State};


handle_info(Info, StateName, State) ->
  yb_common:print_info("stop, info ~p at ~p", [Info, StateName]),
  {stop, "unknow info", State}.

handle_sync_event(Event, _From, StateName, State) ->
  yb_common:print_info("sync event ~p at ~p", [Event, StateName]),
  {next_state, StateName, State}.

handle_event(Event, StateName, State) ->
  yb_common:print_info("event ~p at ~p", [Event, StateName]),
  {next_state, StateName, State}.

terminate(_Reason, StateName, #state{}) ->
  %(catch gen_tcp:close(Socket)),
  yb_common:print_info("[~p] terminate ~p at ~p", [self(), _Reason, StateName]),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}. 

