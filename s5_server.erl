-module(s5_server).
-export([start_sock5_server/0]).

server_host_port() ->
  ["blublu.me", 2345].
  %["106.187.49.213", 2345].

start_sock5_server()->
  [_ServerHost, ServerPort] = server_host_port(),
  {ok, Listen} = gen_tcp:listen(ServerPort, [binary, {packet, 0}, {reuseaddr, true}, {active, true}, {backlog, 128}]),
  loop_listen(Listen).
  %{ok, Socket} = gen_tcp:accept(Listen),
  %gen_tcp:close(Listen),
  %loop(Socket).

loop_listen(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Socket} ->
      spawn(fun() -> loop_listen(Listen) end),
      yb_common:print("new socket :~p~n", [Socket]),
      loop(Socket);

    A -> 
      io:format("loop listen not ok: ~p ~n", [A]),
      start_sock5_server()
  end.

loop(Socket) ->
  receive 
    {tcp, Socket, Bin1} ->
      Bin = yb_common:bin_decode(Bin1),
      back_client_request(Socket, Bin);
    {tcp_closed, Socket} ->
      yb_common:print("Server socket closed~n", []);
    A ->
      yb_common:print("receive msg: ~p ~p ~n", [Socket, A]),
      loop(Socket)
  end.

loop(Socket, Socket2) ->
  receive
    {tcp, Socket, Bin1} ->
      %send to socket2
      Bin = yb_common:bin_decode(Bin1),
      yb_common:print("receive from client: ~p ~p ~n", [Socket, Bin]),
      %io:format("receive from 1: ~p~n", [binary:referenced_byte_size(Bin)]),
      gen_tcp: send(Socket2, Bin),
      loop(Socket, Socket2);
    
    {tcp, Socket2, Bin1} ->
      yb_common:print("receive from server: ~p ~p ~n", [Socket2, Bin1]),
      Bin = yb_common:bin_encode(Bin1),
      %io:format("receive from 2: ~p~n", [binary:referenced_byte_size(Bin)]),
      gen_tcp:send(Socket, Bin),
      loop(Socket, Socket2);

    {tcp_closed, Socket} ->
      yb_common:print("Server socket closed 2~n", []),
      gen_tcp:close(Socket2);

    {tcp_closed, Socket2} ->
      yb_common:print("Server socket closed 3~n", []),
      gen_tcp:close(Socket)
  end.

back_client_request(Socket, Bin) ->
  case Bin of 
    <<_Ver, _Nmethods, _Methods>> ->
      gen_tcp:send(Socket, yb_common:bin_encode(<<5, 0>>)),
      loop(Socket);
    <<_Ver, Cmd, _Rsv1, Atyp, Addr_port/binary>> ->
      [Dst_port, Dst_addr1|[]] = sep_addr_and_port(Addr_port), 
      Dst_addr = yb_common:trim_head(Dst_addr1, yb_common:head(Dst_addr1)),
      yb_common:print("org receive: ~p ~p ~n", [<<_Ver, Cmd, _Rsv1, Atyp>>, Addr_port]),
      yb_common:print("receive: ~p ~p ~n", [<<_Ver, Cmd, _Rsv1, Atyp>>, [Dst_addr, Dst_port]]),
      case Cmd of
        %connect
        1 ->
          yb_common:print("to connect ~p ~p ~n", [Dst_addr, Dst_port]),

          {ok,Socket_connect} = gen_tcp:connect(Dst_addr, Dst_port,[binary, {packet, 0}]),

          yb_common:print("connected to 2 ~p~n", [Socket_connect]),

          gen_tcp:send(Socket, yb_common:bin_encode(<<5, 0, 0, 1, 13,0,0,0, Dst_port:16/integer>>)),

          loop(Socket, Socket_connect);
        A ->
          yb_common:print("not zhichi ~p ~n", [A])

      end;
    A ->
      yb_common:print("unknow request: ~p~n", [A])
  end.

sep_addr_and_port(Addr_port) ->
  [A1,A2|T]  = lists:reverse(binary_to_list(Addr_port)),
  <<Port:16/integer>> = <<A2, A1>>,
  [Port, lists:reverse(T)].

