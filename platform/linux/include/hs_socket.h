#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "sys/types.h"
#include "sys/socket.h"
#include "sys/un.h"
#include "netinet/in.h"
#include <netinet/tcp.h>
#include "netdb.h"

#if defined(_MSC_VER)
#define HS_ALIGNOF(type) __alignof(type)
#else
#define HS_ALIGNOF(type) __alignof__(type)
#endif

int hs_socket  (int domain, int type, int protocol, int *err);
int hs_connect (int fd, const struct sockaddr *name, int namelen, int *err);
int hs_bind    (int fd, const struct sockaddr *name, int namelen, int *err);
int hs_listen  (int fd, int backlog, int *err);
int hs_accept  (int fd, struct sockaddr *addr, int *addrlen, int *err);
int hs_close   (int fd, int *err);

int hs_send    (int fd, const void *buf, size_t len, int flags, int *err);
int hs_recv    (int fd,       void *buf, size_t len, int flags, int *err);
int hs_sendto  (int fd, const void *buf, size_t len, int flags, const struct sockaddr *dest_addr, int addrlen, int *err);
int hs_recvfrom(int fd,       void *buf, size_t len, int flags, struct sockaddr *src_addr, int *addrlen, int *err);

int hs_getsockopt(int fd, int level, int option_name,       void *option_value, int *option_len, int *err);
int hs_setsockopt(int fd, int level, int option_name, const void *option_value, int  option_len, int *err);

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
#if defined(__APPLE__)
#define HS_HAS_SOCKADDR_IN_LEN              1
#define HS_HAS_SOCKADDR_IN6_LEN             1
#define HS_OFFSETOF_SOCKADDR_IN_SIN_LEN     offsetof(struct sockaddr_in, sin_len)
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_LEN   offsetof(struct sockaddr_in6, sin6_len)
#else
#define HS_HAS_SOCKADDR_IN_LEN              0
#define HS_HAS_SOCKADDR_IN6_LEN             0
#define HS_OFFSETOF_SOCKADDR_IN_SIN_LEN     0
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_LEN   0
#endif
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_FAMILY    offsetof(struct sockaddr_in6, sin6_family)
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_PORT      offsetof(struct sockaddr_in6, sin6_port)
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_FLOWINFO  offsetof(struct sockaddr_in6, sin6_flowinfo)
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_SCOPE_ID  offsetof(struct sockaddr_in6, sin6_scope_id)
#define HS_OFFSETOF_SOCKADDR_IN6_SIN6_ADDR      offsetof(struct sockaddr_in6, sin6_addr)
#define HS_OFFSETOF_IN_ADDR_S_ADDR          offsetof(struct in_addr, s_addr)
#define HS_OFFSETOF_IN6_ADDR_S6_ADDR        offsetof(struct in6_addr, s6_addr)

#define SEOK                   0
#define SEINTR                 EINTR
#define SEAGAIN                EAGAIN
#define SEWOULDBLOCK           EWOULDBLOCK
#define SEBADF                 EBADF
#define SEACCES                EACCES
#define SEINVAL                EINVAL
#define SEINPROGRESS           EINPROGRESS
#define SEPROTONOSUPPORT       EPROTONOSUPPORT
#define SECONNREFUSED          ECONNREFUSED
#define SENETUNREACH           ENETUNREACH
#define SENOTCONN              ENOTCONN
#define SEALREADY              EALREADY
#define SEISCONN               EISCONN
#define SETIMEDOUT             ETIMEDOUT
#define SEPIPE                 EPIPE
#define SEOPNOTSUPP            EOPNOTSUPP
#define SENOTSOCK              ENOTSOCK
#define SEDESTADDRREQ          EDESTADDRREQ
#define SEMSGSIZE              EMSGSIZE
#define SEPROTOTYPE            EPROTOTYPE
#define SENOPROTOOPT           ENOPROTOOPT
#define SESOCKTNOSUPPORT       ESOCKTNOSUPPORT
#define SEPFNOSUPPORT          EPFNOSUPPORT
#define SEAFNOSUPPORT          EAFNOSUPPORT
#define SEADDRINUSE            EADDRINUSE
#define SEADDRNOTAVAIL         EADDRNOTAVAIL
#define SENETDOWN              ENETDOWN
#define SENETRESET             ENETRESET
#define SECONNABORTED          ECONNABORTED
#define SECONNRESET            ECONNRESET
#define SENOBUFS               ENOBUFS
#define SESHUTDOWN             ESHUTDOWN
#define SETOOMANYREFS          ETOOMANYREFS
#define SEHOSTDOWN             EHOSTDOWN
#define SEHOSTUNREACH          EHOSTUNREACH

/* MSG_NOSIGNAL might not be available (i.e. on MacOSX and Solaris).
 *   In this case it gets defined as 0. This is relatively
 *   safe to do as the GHC runtime ignores signals that aren't hooked.
 *   The application won't die, but might be unncessarily interrupted.
 */
#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL           0
#endif
