{-# LANGUAGE CApiFFI #-}

module System.Socket.Internal.Constants where

import Foreign.C.Types (CInt (..), CSize (..))

-- Address families
foreign import capi "hs_socket.h value AF_INET"
  c_AF_INET :: CInt

foreign import capi "hs_socket.h value AF_INET6"
  c_AF_INET6 :: CInt

-- Socket types
foreign import capi "hs_socket.h value SOCK_STREAM"
  c_SOCK_STREAM :: CInt

foreign import capi "hs_socket.h value SOCK_DGRAM"
  c_SOCK_DGRAM :: CInt

foreign import capi "hs_socket.h value SOCK_RAW"
  c_SOCK_RAW :: CInt

foreign import capi "hs_socket.h value SOCK_SEQPACKET"
  c_SOCK_SEQPACKET :: CInt

-- Protocol numbers
foreign import capi "hs_socket.h value IPPROTO_TCP"
  c_IPPROTO_TCP :: CInt

foreign import capi "hs_socket.h value IPPROTO_UDP"
  c_IPPROTO_UDP :: CInt

foreign import capi "hs_socket.h value IPPROTO_IPV6"
  c_IPPROTO_IPV6 :: CInt

-- Protocol options
foreign import capi "hs_socket.h value TCP_NODELAY"
  c_TCP_NODELAY :: CInt

foreign import capi "hs_socket.h value IPV6_V6ONLY"
  c_IPV6_V6ONLY :: CInt

-- Socket options
foreign import capi "hs_socket.h value SOL_SOCKET"
  c_SOL_SOCKET :: CInt

foreign import capi "hs_socket.h value SO_ERROR"
  c_SO_ERROR :: CInt

foreign import capi "hs_socket.h value SO_REUSEADDR"
  c_SO_REUSEADDR :: CInt

foreign import capi "hs_socket.h value SO_KEEPALIVE"
  c_SO_KEEPALIVE :: CInt

-- Message flags
foreign import capi "hs_socket.h value MSG_NOSIGNAL"
  c_MSG_NOSIGNAL :: CInt

foreign import capi "hs_socket.h value MSG_EOR"
  c_MSG_EOR :: CInt

foreign import capi "hs_socket.h value MSG_OOB"
  c_MSG_OOB :: CInt

foreign import capi "hs_socket.h value MSG_WAITALL"
  c_MSG_WAITALL :: CInt

foreign import capi "hs_socket.h value MSG_PEEK"
  c_MSG_PEEK :: CInt

-- gai/eai constants
foreign import capi "hs_socket.h value EAI_AGAIN"
  c_EAI_AGAIN :: CInt

foreign import capi "hs_socket.h value EAI_BADFLAGS"
  c_EAI_BADFLAGS :: CInt

foreign import capi "hs_socket.h value EAI_FAIL"
  c_EAI_FAIL :: CInt

foreign import capi "hs_socket.h value EAI_FAMILY"
  c_EAI_FAMILY :: CInt

foreign import capi "hs_socket.h value EAI_MEMORY"
  c_EAI_MEMORY :: CInt

foreign import capi "hs_socket.h value EAI_NONAME"
  c_EAI_NONAME :: CInt

foreign import capi "hs_socket.h value EAI_SERVICE"
  c_EAI_SERVICE :: CInt

foreign import capi "hs_socket.h value EAI_SOCKTYPE"
  c_EAI_SOCKTYPE :: CInt

foreign import capi "hs_socket.h value EAI_SYSTEM"
  c_EAI_SYSTEM :: CInt

-- Address info flags
foreign import capi "hs_socket.h value AI_ADDRCONFIG"
  c_AI_ADDRCONFIG :: CInt

foreign import capi "hs_socket.h value AI_ALL"
  c_AI_ALL :: CInt

foreign import capi "hs_socket.h value AI_CANONNAME"
  c_AI_CANONNAME :: CInt

foreign import capi "hs_socket.h value AI_NUMERICHOST"
  c_AI_NUMERICHOST :: CInt

foreign import capi "hs_socket.h value AI_NUMERICSERV"
  c_AI_NUMERICSERV :: CInt

foreign import capi "hs_socket.h value AI_PASSIVE"
  c_AI_PASSIVE :: CInt

foreign import capi "hs_socket.h value AI_V4MAPPED"
  c_AI_V4MAPPED :: CInt

-- Name info flags and limits
foreign import capi "hs_socket.h value NI_NAMEREQD"
  c_NI_NAMEREQD :: CInt

foreign import capi "hs_socket.h value NI_DGRAM"
  c_NI_DGRAM :: CInt

foreign import capi "hs_socket.h value NI_NOFQDN"
  c_NI_NOFQDN :: CInt

foreign import capi "hs_socket.h value NI_NUMERICHOST"
  c_NI_NUMERICHOST :: CInt

foreign import capi "hs_socket.h value NI_NUMERICSERV"
  c_NI_NUMERICSERV :: CInt

foreign import capi "hs_socket.h value NI_MAXHOST"
  c_NI_MAXHOST :: CInt

foreign import capi "hs_socket.h value NI_MAXSERV"
  c_NI_MAXSERV :: CInt

-- SocketException codes
foreign import capi "hs_socket.h value SEOK"
  c_SEOK :: CInt

foreign import capi "hs_socket.h value SEINTR"
  c_SEINTR :: CInt

foreign import capi "hs_socket.h value SEAGAIN"
  c_SEAGAIN :: CInt

foreign import capi "hs_socket.h value SEWOULDBLOCK"
  c_SEWOULDBLOCK :: CInt

foreign import capi "hs_socket.h value SEBADF"
  c_SEBADF :: CInt

foreign import capi "hs_socket.h value SEACCES"
  c_SEACCES :: CInt

foreign import capi "hs_socket.h value SEINVAL"
  c_SEINVAL :: CInt

foreign import capi "hs_socket.h value SEPIPE"
  c_SEPIPE :: CInt

foreign import capi "hs_socket.h value SENOTSOCK"
  c_SENOTSOCK :: CInt

foreign import capi "hs_socket.h value SEDESTADDRREQ"
  c_SEDESTADDRREQ :: CInt

foreign import capi "hs_socket.h value SEMSGSIZE"
  c_SEMSGSIZE :: CInt

foreign import capi "hs_socket.h value SEPROTOTYPE"
  c_SEPROTOTYPE :: CInt

foreign import capi "hs_socket.h value SENOPROTOOPT"
  c_SENOPROTOOPT :: CInt

foreign import capi "hs_socket.h value SEPROTONOSUPPORT"
  c_SEPROTONOSUPPORT :: CInt

foreign import capi "hs_socket.h value SESOCKTNOSUPPORT"
  c_SESOCKTNOSUPPORT :: CInt

foreign import capi "hs_socket.h value SEOPNOTSUPP"
  c_SEOPNOTSUPP :: CInt

foreign import capi "hs_socket.h value SEPFNOSUPPORT"
  c_SEPFNOSUPPORT :: CInt

foreign import capi "hs_socket.h value SEAFNOSUPPORT"
  c_SEAFNOSUPPORT :: CInt

foreign import capi "hs_socket.h value SEADDRINUSE"
  c_SEADDRINUSE :: CInt

foreign import capi "hs_socket.h value SEADDRNOTAVAIL"
  c_SEADDRNOTAVAIL :: CInt

foreign import capi "hs_socket.h value SENETDOWN"
  c_SENETDOWN :: CInt

foreign import capi "hs_socket.h value SENETUNREACH"
  c_SENETUNREACH :: CInt

foreign import capi "hs_socket.h value SENETRESET"
  c_SENETRESET :: CInt

foreign import capi "hs_socket.h value SECONNABORTED"
  c_SECONNABORTED :: CInt

foreign import capi "hs_socket.h value SECONNRESET"
  c_SECONNRESET :: CInt

foreign import capi "hs_socket.h value SENOBUFS"
  c_SENOBUFS :: CInt

foreign import capi "hs_socket.h value SEISCONN"
  c_SEISCONN :: CInt

foreign import capi "hs_socket.h value SENOTCONN"
  c_SENOTCONN :: CInt

foreign import capi "hs_socket.h value SESHUTDOWN"
  c_SESHUTDOWN :: CInt

foreign import capi "hs_socket.h value SETOOMANYREFS"
  c_SETOOMANYREFS :: CInt

foreign import capi "hs_socket.h value SETIMEDOUT"
  c_SETIMEDOUT :: CInt

foreign import capi "hs_socket.h value SECONNREFUSED"
  c_SECONNREFUSED :: CInt

foreign import capi "hs_socket.h value SEHOSTDOWN"
  c_SEHOSTDOWN :: CInt

foreign import capi "hs_socket.h value SEHOSTUNREACH"
  c_SEHOSTUNREACH :: CInt

foreign import capi "hs_socket.h value SEALREADY"
  c_SEALREADY :: CInt

foreign import capi "hs_socket.h value SEINPROGRESS"
  c_SEINPROGRESS :: CInt

-- Structure sizes
foreign import capi "hs_socket.h value HS_SIZEOF_ADDRINFO"
  c_sizeof_addrinfo :: CSize

foreign import capi "hs_socket.h value HS_SIZEOF_SOCKADDR_IN"
  c_sizeof_sockaddr_in :: CSize

foreign import capi "hs_socket.h value HS_SIZEOF_SOCKADDR_IN6"
  c_sizeof_sockaddr_in6 :: CSize

-- Alignments
foreign import capi "hs_socket.h value HS_ALIGNOF_SOCKADDR_IN"
  c_alignof_sockaddr_in :: CSize

foreign import capi "hs_socket.h value HS_ALIGNOF_SOCKADDR_IN6"
  c_alignof_sockaddr_in6 :: CSize

-- Offsets for struct addrinfo
foreign import capi "hs_socket.h value HS_OFFSETOF_ADDRINFO_AI_FLAGS"
  c_offset_addrinfo_ai_flags :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_ADDRINFO_AI_FAMILY"
  c_offset_addrinfo_ai_family :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_ADDRINFO_AI_SOCKTYPE"
  c_offset_addrinfo_ai_socktype :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_ADDRINFO_AI_PROTOCOL"
  c_offset_addrinfo_ai_protocol :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_ADDRINFO_AI_ADDR"
  c_offset_addrinfo_ai_addr :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_ADDRINFO_AI_CANONNAME"
  c_offset_addrinfo_ai_canonname :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_ADDRINFO_AI_NEXT"
  c_offset_addrinfo_ai_next :: CSize

-- Offsets for struct sockaddr_in
foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN_SIN_FAMILY"
  c_offset_sockaddr_in_sin_family :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN_SIN_PORT"
  c_offset_sockaddr_in_sin_port :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN_SIN_ADDR"
  c_offset_sockaddr_in_sin_addr :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN_SIN_LEN"
  c_offset_sockaddr_in_sin_len :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_IN_ADDR_S_ADDR"
  c_offset_in_addr_s_addr :: CSize

-- Offsets for struct sockaddr_in6
foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN6_SIN6_FAMILY"
  c_offset_sockaddr_in6_sin6_family :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN6_SIN6_PORT"
  c_offset_sockaddr_in6_sin6_port :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN6_SIN6_FLOWINFO"
  c_offset_sockaddr_in6_sin6_flowinfo :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN6_SIN6_SCOPE_ID"
  c_offset_sockaddr_in6_sin6_scope_id :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN6_SIN6_ADDR"
  c_offset_sockaddr_in6_sin6_addr :: CSize

foreign import capi "hs_socket.h value HS_OFFSETOF_SOCKADDR_IN6_SIN6_LEN"
  c_offset_sockaddr_in6_sin6_len :: CSize

-- Offset within struct in6_addr
foreign import capi "hs_socket.h value HS_OFFSETOF_IN6_ADDR_S6_ADDR"
  c_offset_in6_addr_s6_addr :: CSize

foreign import capi "hs_socket.h value HS_HAS_SOCKADDR_IN_LEN"
  c_has_sockaddr_in_len :: CInt

foreign import capi "hs_socket.h value HS_HAS_SOCKADDR_IN6_LEN"
  c_has_sockaddr_in6_len :: CInt
