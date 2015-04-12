-module(s5_client_handler).
-behaviour(gen_server).
-include("s5_records.hrl").
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]). 

-record(state, {lsock, client_socket, server_socket, addr, server_conf, workstat}).

start_link(LSock, #server_conf{} = ServerConf) ->
  %gen_server:start_link(Module, Args, Options) -> Result
  gen_server:start_link(?MODULE, [LSock, ServerConf], []).

init([LSocket, ServerConf]) ->
  %inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
  {ok, #state{lsock = LSocket, server_conf = ServerConf, workstat = 0}, 0}. %{ok,State,Timeout}

handle_call(Msg, _From, State) ->
  {reply, {ok, Msg}, State}.
 
handle_cast(stop, State) ->
  {stop, normal, State}. 

handle_info({tcp, ClientSocket, Data}, #state{client_socket = ClientSocket, server_socket = ServerSocket} = State) ->
  %inet:setopts(Socket, [{active, once}]),
  %yb_common:print_info("from client ~p", [Data]),
  ok = gen_tcp: send(ServerSocket, yb_common:bin_encode(Data)),
  {noreply, State, 5000};

handle_info({tcp, ServerSocket, Data}, #state{client_socket = ClientSocket, server_socket = ServerSocket} = State) ->
  %inet:setopts(Socket, [{active, once}]),
  %yb_common:print_info("from server ~p", [Data]),
  ok = gen_tcp:send(ClientSocket, yb_common:bin_decode(Data)),
  {noreply, State, 5000}; 

handle_info({tcp_closed, ClientSocket}, #state{client_socket = ClientSocket, server_socket = ServerSocket} = State) ->
  gen_tcp:close(ServerSocket),
  {stop, "client close", State}; 

handle_info({tcp_closed, ServerSocket}, #state{client_socket = ClientSocket, server_socket = ServerSocket} = State) ->
  gen_tcp:close(ClientSocket),
  {stop, "server close", State}; 

handle_info(timeout, #state{client_socket = ClientSocket, server_socket = ServerSocket, workstat = 1} = State) ->
  gen_tcp:close(ServerSocket),
  gen_tcp:close(ClientSocket),
  {stop, "time out", State};

handle_info(timeout, #state{lsock = LSock, server_conf = ServerConf, workstat = 0} = State) ->
  {ok, ClientSocket} = gen_tcp:accept(LSock),
  %{ok, {IP, _Port}} = inet:peername(ClientSocket),
  s5_client_sup:start_child(),
  %connect to server
  case gen_tcp:connect(ServerConf#server_conf.host, ServerConf#server_conf.port, [binary, {packet, 0}]) of
    {ok, ServerSocket} ->
      yb_common:print("match socket :client ~p server ~p~n", [ClientSocket, ServerSocket]),
      {noreply, State#state{server_socket=ServerSocket, client_socket=ClientSocket, workstat = 1}}; 
    A ->
      yb_common:print_error("[error] connect to server error: ~p ~n", [A]),
      gen_tcp:close(ClientSocket),
      {stop, "connect to server error", State}
  end;

handle_info(_Info, State) ->
  yb_common:print_info("info ~p", [_Info]),
  {noreply, State}. 

terminate(_Reason, #state{}) ->
  %(catch gen_tcp:close(Socket)),
  yb_common:print_info("terminate ~p", [_Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}. 

