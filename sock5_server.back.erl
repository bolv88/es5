-module(sock5_server).
-export([start_sock5_server/0,start_nano_client/0, sleep/1, trim_head/2]).

start_sock5_server()->
  {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
  loop_listen(Listen).
  %{ok, Socket} = gen_tcp:accept(Listen),
  %gen_tcp:close(Listen),
  %loop(Socket).

loop_listen(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> loop_listen(Listen) end),
  io:format("new socket :~p~n", [Socket]),
  loop(Socket).

loop(Socket) ->
  receive 
    {tcp, Socket, Bin} ->
      back_client_request(Socket, Bin);
    {tcp_closed, Socket} ->
      io:format("Server socket closed~n");
    A ->
      io:format("receive msg: ~p ~p ~n", [Socket, A]),
      loop(Socket)
  end.

loop(Socket, Socket2) ->
  receive
    {tcp, Socket, Bin} ->
      %send to socket2
      io:format("receive from 1: ~p~n", [Bin]),
      gen_tcp: send(Socket2, Bin),
      loop(Socket, Socket2);
    
    {tcp, Socket2, Bin} ->
      io:format("receive from 2: ~p~n", [Bin]),
      gen_tcp: send(Socket, Bin),
      loop(Socket, Socket2);

    {tcp_closed, Socket} ->
      io:format("Server socket closed 2~n");

    {tcp_closed, Socket2} ->
      io:format("Server socket closed 3~n")
  end.

back_client_request(Socket, Bin) ->
  case Bin of 
    <<_Ver, _Nmethods, _Methods>> ->
      gen_tcp:send(Socket, <<5, 0>>),
      loop(Socket);
    <<_Ver, Cmd, _Rsv1, Atyp, Addr_port/binary>> ->
      [Dst_port, Dst_addr1|[]] = sep_addr_and_port(Addr_port), 
      Dst_addr = trim_head(Dst_addr1, head(Dst_addr1)),
      io:format("org receive: ~p ~p ~n", [<<_Ver, Cmd, _Rsv1, Atyp>>, Addr_port]),
      io:format("receive: ~p ~p ~n", [<<_Ver, Cmd, _Rsv1, Atyp>>, [Dst_addr, Dst_port]]),
      case Cmd of
        %connect
        1 ->
          io:format("to connect ~p ~p ~n", [Dst_addr, 80]),

          {ok,Socket_connect} = gen_tcp:connect(Dst_addr,80,[binary, {packet, 0}]),

          io:format("connected to 2 ~p~n", [Socket_connect]),

          gen_tcp:send(Socket, <<5, 0, 0, 1, 13,0,0,0, 0,80>>),

          loop(Socket, Socket_connect);
        A ->
          io:format("not zhichi ~p ~n", [A])

      end;
    A ->
      io:format("unknow request: ~p~n", [A])
  end.

sep_addr_and_port(Addr_port) ->
  [A1,A2|T]  = lists:reverse(binary_to_list(Addr_port)),
  [[A2, A1], lists:reverse(T)].

start_nano_client()->
  {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, term_to_binary({1,2})),
  receive 
    {tcp, Socket, Bin} ->
      io:format("Client received binary = ~p ~n", [Bin]),
      Val = binary_to_term(Bin),
      io:format("Client received unpacked = ~p ~n", [Val]),
      gen_tcp:close(Socket)
  end.

sleep(T) ->
  receive
  after T ->
      true
  end.

head(L) ->
  [H|T] = L,
  H.

trim_head(L, H) ->
  case L of 
    [H|T] ->
      trim_head(T, H);
    L1 ->
      L1
  end.
