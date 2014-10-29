-module(sock5_server).
-export([start_sock5_server/0, sleep/1, trim_head/2, bin_encode/1, bin_decode/1, start_sock5_client/0]).

server_host_port() ->
  ["blublu.me", 2345].
  %["106.187.49.213", 2345].

start_sock5_client()->
  [_ServerHost, ServerPort] = server_host_port(),
  {ok, Listen} = gen_tcp:listen(ServerPort, [binary, {packet, 0}, {reuseaddr, true}, {active, true}, {backlog, 128}]),
  client_loop_listen(Listen).

client_loop_listen(Listen) ->
  [ServerHost, ServerPort] = server_host_port(),

  case gen_tcp:accept(Listen) of 
    {ok, Socket} ->
      io:format("accept ~p ~n", [Socket]),
      spawn(fun() -> client_loop_listen(Listen) end),
      case gen_tcp:connect(ServerHost, ServerPort,[binary, {packet, 0}]) of
        {ok,Socket_connect} ->
            print("match socket :client ~p server ~p~n", [Socket, Socket_connect]),
            client_loop(Socket, Socket_connect);
        A ->
            io:format("[error] connect to server error: ~p ~n", [A]),
            gen_tcp:close(Socket),
            client_loop_listen(Listen)
      end;

    {error,closed} ->
      io:format("error, closed"),
      start_sock5_client();

    A ->
      io:format("client loop listen error: ~p ~n", [A]),
      client_loop_listen(Listen)
  end.


client_loop(SocketBrowser, SocketServer) -> 
  receive
    {tcp, SocketBrowser, Bin} ->
      %io:format("from browser: ~p ~p ~n", [SocketBrowser, binary:referenced_byte_size(Bin)]),
      io:format("from browser: ~p ~p ~n", [SocketBrowser, Bin]),
      gen_tcp: send(SocketServer, bin_encode(Bin)),
      client_loop(SocketBrowser, SocketServer);

    {tcp, SocketServer, Bin1} ->
      io:format("from server: ~p ~n", [binary:referenced_byte_size(Bin1)]),
      %io:format("from server: ~p ~p ~n", [SocketBrowser, Bin1]),
      Bin = bin_decode(Bin1),
      %<<A1:1/binary, A2:1/binary, A3/binary>> = Bin,
      %io:format("from server: ~p ~n", [<<A1, A2>>]),
      print("from server: ~p ~p ~n", [SocketServer, Bin]),
      gen_tcp: send(SocketBrowser, Bin),
      client_loop(SocketBrowser, SocketServer);

    {tcp_closed, SocketBrowser} ->
      gen_tcp:close(SocketServer);

    {tcp_closed, SocketServer} ->
      gen_tcp:close(SocketBrowser)
  end.

  
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
      print("new socket :~p~n", [Socket]),
      loop(Socket);

    A -> 
      io:format("loop listen not ok: ~p ~n", [A]),
      start_sock5_server()
  end.

loop(Socket) ->
  receive 
    {tcp, Socket, Bin1} ->
      Bin = bin_decode(Bin1),
      back_client_request(Socket, Bin);
    {tcp_closed, Socket} ->
      print("Server socket closed~n", []);
    A ->
      print("receive msg: ~p ~p ~n", [Socket, A]),
      loop(Socket)
  end.

loop(Socket, Socket2) ->
  receive
    {tcp, Socket, Bin1} ->
      %send to socket2
      Bin = bin_decode(Bin1),
      print("receive from client: ~p ~p ~n", [Socket, Bin]),
      %io:format("receive from 1: ~p~n", [binary:referenced_byte_size(Bin)]),
      gen_tcp: send(Socket2, Bin),
      loop(Socket, Socket2);
    
    {tcp, Socket2, Bin1} ->
      print("receive from server: ~p ~p ~n", [Socket2, Bin1]),
      Bin = bin_encode(Bin1),
      %io:format("receive from 2: ~p~n", [binary:referenced_byte_size(Bin)]),
      gen_tcp:send(Socket, Bin),
      loop(Socket, Socket2);

    {tcp_closed, Socket} ->
      print("Server socket closed 2~n", []),
      gen_tcp:close(Socket2);

    {tcp_closed, Socket2} ->
      print("Server socket closed 3~n", []),
      gen_tcp:close(Socket)
  end.

back_client_request(Socket, Bin) ->
  case Bin of 
    <<_Ver, _Nmethods, _Methods>> ->
      gen_tcp:send(Socket, bin_encode(<<5, 0>>)),
      loop(Socket);
    <<_Ver, Cmd, _Rsv1, Atyp, Addr_port/binary>> ->
      [Dst_port, Dst_addr1|[]] = sep_addr_and_port(Addr_port), 
      Dst_addr = trim_head(Dst_addr1, head(Dst_addr1)),
      print("org receive: ~p ~p ~n", [<<_Ver, Cmd, _Rsv1, Atyp>>, Addr_port]),
      print("receive: ~p ~p ~n", [<<_Ver, Cmd, _Rsv1, Atyp>>, [Dst_addr, Dst_port]]),
      case Cmd of
        %connect
        1 ->
          print("to connect ~p ~p ~n", [Dst_addr, Dst_port]),

          {ok,Socket_connect} = gen_tcp:connect(Dst_addr, Dst_port,[binary, {packet, 0}]),

          print("connected to 2 ~p~n", [Socket_connect]),

          gen_tcp:send(Socket, bin_encode(<<5, 0, 0, 1, 13,0,0,0, Dst_port:16/integer>>)),

          loop(Socket, Socket_connect);
        A ->
          print("not zhichi ~p ~n", [A])

      end;
    A ->
      print("unknow request: ~p~n", [A])
  end.

sep_addr_and_port(Addr_port) ->
  [A1,A2|T]  = lists:reverse(binary_to_list(Addr_port)),
  <<Port:16/integer>> = <<A2, A1>>,
  [Port, lists:reverse(T)].


sleep(T) ->
  receive
  after T ->
      true
  end.

head(L) ->
  [H|_T] = L,
  H.

trim_head(L, H) ->
  case L of 
    [H|T] ->
      trim_head(T, H);
    L1 ->
      L1
  end.

bin_encode(Bin) ->
  bin_encode(Bin, []).

bin_encode(<<>>, L) ->
  list_to_binary(lists:reverse(L));
bin_encode(<<H:8/integer, T/binary>>, L) -> 
  bin_encode(T, [ 255 - H |  L]).


bin_decode(Bin) ->
  bin_decode(Bin, []).

bin_decode(<<>>, L) ->
  list_to_binary(lists:reverse(L));
bin_decode(<<H:8/integer, T/binary>>, L) ->
  bin_decode(T, [255 - H | L]).

print(_Format, _L) ->
  %io:format(_Format, _L).
  1.
