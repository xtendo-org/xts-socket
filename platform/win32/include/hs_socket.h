#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <winsock2.h>

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif
#if (_WIN32_WINNT < 0x0501)
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif

#include <ws2tcpip.h>

#if defined(_MSC_VER)
#define HS_ALIGNOF(type) __alignof(type)
#else
#define HS_ALIGNOF(type) __alignof__(type)
#endif

#ifndef MSG_EOR
#define MSG_EOR 0
#endif

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

#ifndef MSG_WAITALL
#define MSG_WAITALL 0
#endif

#ifndef EAI_SYSTEM
#define EAI_SYSTEM 0
#endif

/* Value for this flag taken from the former Haskell network
   library. Remove if MinGW fixed this.
*/
#ifndef IPV6_V6ONLY
#define IPV6_V6ONLY 27
#endif

/* Values for this flags taken from
   http://sourceforge.net/p/mingw-w64/mailman/message/33056995/.
   According to MSDN documentation they are supported on
   Windows Vista or higher. This definitions may be removed
   when MinGW finally ships them.
*/

#ifndef AI_PASSIVE
#define AI_PASSIVE                  0x00000001
#endif
#ifndef AI_CANONNAME
#define AI_CANONNAME                0x00000002
#endif
#ifndef AI_NUMERICHOST
#define AI_NUMERICHOST              0x00000004
#endif
#ifndef AI_NUMERICSERV
#define AI_NUMERICSERV              0x00000008
#endif
#ifndef AI_ALL
#define AI_ALL                      0x00000100
#endif
#ifndef AI_ADDRCONFIG
#define AI_ADDRCONFIG               0x00000400
#endif
#ifndef AI_V4MAPPED
#define AI_V4MAPPED                 0x00000800
#endif
#ifndef AI_NON_AUTHORITATIVE
#define AI_NON_AUTHORITATIVE        0x00004000
#endif
#ifndef AI_SECURE
#define AI_SECURE                   0x00008000
#endif
#ifndef AI_RETURN_PREFERRED_NAMES
#define AI_RETURN_PREFERRED_NAMES   0x00010000
#endif

int hs_get_last_socket_error();

int hs_socket_init();

int hs_socket  (int domain, int type, int protocol, int *err);
int hs_bind    (int sockfd, const struct sockaddr *name, int namelen, int *err);
int hs_connect (int sockfd, const struct sockaddr *name, int namelen, int *err);
int hs_connect_status (int sockfd, int *err);
int hs_listen  (int sockfd, int backlog, int *err);
int hs_accept  (int sockfd, struct sockaddr *addr, int *addrlen, int *err);
int hs_close   (int sockfd, int *err);

int hs_send    (int sockfd, const void *buf, size_t len, int flags, int *err);
int hs_recv    (int sockfd,       void *buf, size_t len, int flags, int *err);
int hs_sendto  (int sockfd, const void *buf, size_t len, int flags, const struct sockaddr *dest_addr, int addrlen, int *err);
int hs_recvfrom(int sockfd,       void *buf, size_t len, int flags, struct sockaddr *src_addr, int *addrlen, int *err);

int hs_getsockopt(int sockfd, int level, int option_name,       void *option_value, int *option_len, int *err);
int hs_setsockopt(int sockfd, int level, int option_name, const void *option_value, int  option_len, int *err);

#define HS_SIZEOF_ADDRINFO                  (sizeof(struct addrinfo))
#define HS_SIZEOF_SOCKADDR_IN               (sizeof(struct sockaddr_in))
#define HS_SIZEOF_SOCKADDR_IN6              (sizeof(struct sockaddr_in6))
#define HS_ALIGNOF_SOCKADDR_IN              HS_ALIGNOF(struct sockaddr_in)
#define HS_ALIGNOF_SOCKADDR_IN6             HS_ALIGNOF(struct sockaddr_in6)
#define HS_OFFSETOF_ADDRINFO_AI_FLAGS       offsetof(struct addrinfo, ai_flags)
#define HS_OFFSETOF_ADDRINFO_AI_FAMILY      offsetof(struct addrinfo, ai_family)
#define HS_OFFSETOF_ADDRINFO_AI_SOCKTYPE    offsetof(struct addrinfo, ai_socktype)
#define HS_OFFSETOF_ADDRINFO_AI_PROTOCOL    offsetof(struct addrinfo, ai_protocol)
#define HS_OFFSETOF_ADDRINFO_AI_ADDR        offsetof(struct addrinfo, ai_addr)
#define HS_OFFSETOF_ADDRINFO_AI_CANONNAME   offsetof(struct addrinfo, ai_canonname)
#define HS_OFFSETOF_ADDRINFO_AI_NEXT        offsetof(struct addrinfo, ai_next)
#define HS_OFFSETOF_SOCKADDR_IN_SIN_FAMILY  offsetof(struct sockaddr_in, sin_family)
#define HS_OFFSETOF_SOCKADDR_IN_SIN_PORT    offsetof(struct sockaddr_in, sin_port)
#define HS_OFFSETOF_SOCKADDR_IN_SIN_ADDR    offsetof(struct sockaddr_in, sin_addr)
#define HS_SIZEOF_SA_FAMILY                 (sizeof(((struct sockaddr_in *)0)->sin_family))
#define HS_SIZEOF_SA_FAMILY6                (sizeof(((struct sockaddr_in6 *)0)->sin6_family))
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_FAMILY    offsetof(struct sockaddr_in6, sin6_family)
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_PORT      offsetof(struct sockaddr_in6, sin6_port)
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_FLOWINFO  offsetof(struct sockaddr_in6, sin6_flowinfo)
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_SCOPE_ID  offsetof(struct sockaddr_in6, sin6_scope_id)
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_ADDR      offsetof(struct sockaddr_in6, sin6_addr)
#define HS_OFFSETOF_IN_ADDR_S_ADDR          offsetof(struct in_addr, s_addr)
#define HS_OFFSETOF_IN6_ADDR_S6_ADDR        offsetof(struct in6_addr, s6_addr)

int  hs_getaddrinfo(const char *node, const char *service,
                    const struct addrinfo *hints,
                    struct addrinfo **res);

int  hs_getnameinfo(const struct sockaddr *sa, int salen,
                    char *host, int hostlen,
                    char *serv, int servlen, int flags);

void hs_freeaddrinfo(struct addrinfo *res);

#define SEOK                   0
#define SEINTR                 WSAEINTR
#define SEAGAIN                WSATRY_AGAIN
#define SEWOULDBLOCK           WSAEWOULDBLOCK
#define SEBADF                 WSAEBADF
#define SEACCES                WSAEACCES
#define SEINVAL                WSAEINVAL
#define SEINPROGRESS           WSAEINPROGRESS
#define SEPROTONOSUPPORT       WSAEPROTONOSUPPORT
#define SECONNREFUSED          WSAECONNREFUSED
#define SENETUNREACH           WSAENETUNREACH
#define SENOTCONN              WSAENOTCONN
#define SEALREADY              WSAEALREADY
#define SEISCONN               WSAEISCONN
#define SETIMEDOUT             WSAETIMEDOUT
#define SEPIPE                 WSAECONNABORTED
#define SEOPNOTSUPP            WSAEOPNOTSUPP
#define SENOTSOCK              WSAENOTSOCK
#define SEHOSTUNREACH          WSAEHOSTUNREACH
#define SEHOSTDOWN             WSAEHOSTDOWN
#define SETOOMANYREFS          WSAETOOMANYREFS
#define SESHUTDOWN             WSAESHUTDOWN
#define SENOBUFS               WSAENOBUFS
#define SENETRESET             WSAENETRESET
#define SENETDOWN              WSAENETDOWN
#define SECONNABORTED          WSAECONNABORTED
#define SECONNRESET            WSAECONNRESET
#define SEADDRNOTAVAIL         WSAEADDRNOTAVAIL
#define SEADDRINUSE            WSAEADDRINUSE
#define SEAFNOSUPPORT          WSAEAFNOSUPPORT
#define SEPFNOSUPPORT          WSAEPFNOSUPPORT
#define SESOCKTNOSUPPORT       WSAESOCKTNOSUPPORT
#define SENOPROTOOPT           WSAENOPROTOOPT
#define SEPROTOTYPE            WSAEPROTOTYPE
#define SEMSGSIZE              WSAEMSGSIZE
#define SEDESTADDRREQ          WSAEDESTADDRREQ

// These four constants are always zero on Windows;
// See <https://github.com/xtendo-org/xts-socket/pull/2>
#define HS_HAS_SOCKADDR_IN_LEN              0
#define HS_HAS_SOCKADDR_IN6_LEN             0
#define HS_OFFSETOF_SOCKADDR_IN_SIN_LEN     0
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_LEN   0
