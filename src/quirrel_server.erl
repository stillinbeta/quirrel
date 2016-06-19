-module(quirrel_server).

-include("src/quirrel.hrl").

-export([start/0, loop/1]).

start() ->
  spawn(fun() -> server() end).

server() ->
  Port = application:get_env(quirrel, udp_port, 4000),
  io:format("server started on port ~p~n", [Port]),
  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
  loop(Socket).

loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, _Socket, _Host, _Port, Bin} ->
      %% io:format("server received from ~p:~p value ~p~n", [Host, Port, Bin]),
      %% Allow live reloads
      {ok, PacketHeader, PacketType, Packet} = quirrel_parse_frame:parse_headers(Bin, client),
      io:format("Headers: ~p", [PacketHeader]),
      io:format("Packet: ~p~n", [quirrel_parse_frame:parse_packet(PacketType, PacketHeader, Packet)]),
      ?MODULE:loop(Socket)
  end.

