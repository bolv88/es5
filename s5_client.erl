-module(s5_client).
-behaviour(gen_server).
-export([start_sock5_client/0]).

client_port()->
  3456.
server_host_port() ->
  %["blublu.me", 2345].
  ["127.0.0.1", 2345].
  %["106.187.49.213", 2345].

start_sock5_client()->
  ServerPort = client_port(),
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
          yb_common:print("match socket :client ~p server ~p~n", [Socket, Socket_connect]),
          client_loop(Socket, Socket_connect);
        A ->
            io:format("[error] connect to server error: ~p ~n", [A]),
            gen_tcp:close(Socket),
            client_loop_listen(Listen)
      end;

    {error,closed} ->
      io:format("error, closed ~n"),
      start_sock5_client();

    A ->
      io:format("client loop listen error: ~p ~n", [A]),
      client_loop_listen(Listen)
  end.


client_loop(SocketBrowser, SocketServer) -> 
  receive
    {tcp, SocketBrowser, Bin} ->
      %io:format("from browser: ~p ~p ~n", [SocketBrowser, binary:referenced_byte_size(Bin)]),
      yb_common:print_info("-> from browser: ~p ~p", [SocketBrowser, binary:referenced_byte_size(Bin)]),
      gen_tcp: send(SocketServer, yb_common:bin_encode(Bin)),
      client_loop(SocketBrowser, SocketServer);

    {tcp, SocketServer, Bin1} ->
      %io:format("from server: ~p ~p ~n", [SocketBrowser, Bin1]),
      Bin = yb_common:bin_decode(Bin1),
      %<<A1:1/binary, A2:1/binary, A3/binary>> = Bin,
      %io:format("from server: ~p ~n", [<<A1, A2>>]),
      yb_common:print("<- from server: ~p ~p ~n", [SocketServer, binary:referenced_byte_size(Bin)]),
      gen_tcp: send(SocketBrowser, Bin),
      client_loop(SocketBrowser, SocketServer);

    {tcp_closed, SocketBrowser} ->
      gen_tcp:close(SocketServer);

    {tcp_closed, SocketServer} ->
      gen_tcp:close(SocketBrowser)
  end.

handle_call(Request, From, State) ->
  {reply, Reply, State}.

handle_info(Info, State) ->
  {noreply, State}.
  
terminate(Reason, State) ->
  ok.

code_change(OldVsn, State, Extra) ->
  {ok, State}.
