-record(packet_header, {connection_id   :: integer() | undefined
                       ,connection_bits :: integer()
                       ,quic_version    :: quic_version()  | undefined
                       ,packet_number   :: integer() | undefined
                       }).

-type packet_type() :: regular | reset | version.
-type packet() :: binary().
-type quic_version() :: <<_:32>>.


