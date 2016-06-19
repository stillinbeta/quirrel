-module(quirrel_parse_frame).

-include("src/quirrel.hrl").

-export([parse_headers/2, parse_packet/3]).

-spec parse_headers(binary(), server | client) -> {ok, #packet_header{}, packet_type(), packet()} | {error, term()}.
parse_headers(PacketWithHeaders, ServerOrClient) ->
  <<0:1, _Multipath:1, PacketNumberBytes:2, ConnectionIdBytes:2, FlagReset:1,
    FlagVersion:1, Rest/binary>> = PacketWithHeaders,
  {ConnectionIdSize, ConnectionIdPresent} = case ConnectionIdBytes of
                                              3 -> {64, true}; % 0x0c
                                              2 -> {32, true}; % 0x08
                                              1 -> {8,  true}; % 0x04
                                              0 -> {0, false}  % 0x00
                                            end,
  PacketNumberSize = case PacketNumberBytes of
                     3 -> 48; % 0x30 -> 6 bytes
                     2 -> 32; % 0x20 -> 4 bytes
                     1 -> 16; % 0x10 -> 2 bytes
                     0 -> 8   % 0x00 -> 1 byte
                   end,
  {VersionSize, VersionPresent} = case {FlagVersion, ServerOrClient} of
                                    {1, client} -> {4, true};
                                    {_, _}      -> {0, false}
                                  end,
  Type = case {FlagReset, FlagVersion, ServerOrClient} of
               {1, _, _} -> reset;
               {_, 0, _} -> regular;
               {_, 1, server} -> version;
               {_, 1, client} -> regular
             end,

  io:format("bit_size() -> ~p~n", [bit_size(Rest)]),
  case {Type, Rest} of
      %% For version packet, version is part of packet "body"
    {version,
       <<ConnectionId:ConnectionIdSize/little-integer, Packet/binary>>} ->
        {ok, #packet_header{connection_id = ConnectionId
                           ,connection_bits = ConnectionIdSize
                           ,quic_version = undefined
                           ,packet_number = undefined
                           }, Type, Packet};
    {_, <<ConnectionId:ConnectionIdSize/little-integer
         ,Version:VersionSize/binary
         ,PacketNumber:PacketNumberSize/integer
         ,Packet/binary
        >>} ->
            {ok, #packet_header{connection_id = case ConnectionIdPresent of
                                    true  -> ConnectionId;
                                    false -> undefined
                                  end
                               ,connection_bits = ConnectionIdSize
                               ,quic_version = case VersionPresent of
                                                 true  -> Version;
                                                 false -> undefined
                                               end
                               ,packet_number = PacketNumber
                               }, Type, Packet};
    _NoParse -> {error, no_parse_sorry}
  end.

-spec parse_packet(packet_type(), #packet_header{}, packet()) -> {ok, term()}.
parse_packet(version, #packet_header{connection_bits=64}, Packet) ->
  {ok, parse_version_packet(Packet, [])};
parse_packet(PacketType, _, _) -> {error, {no_parse, PacketType}}.

%% TODO EKF: sometimes there's just random memory here????
parse_version_packet(<<Version:4/binary, Rest/binary>>, Acc) ->
  parse_version_packet(Rest, [Version|Acc]);
parse_version_packet(_, Acc) -> lists:reverse(Acc).




