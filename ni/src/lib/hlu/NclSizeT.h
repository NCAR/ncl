#ifndef _NCLSIZET_H
#define _NCLSIZET_H

#ifdef NCL32BITS
typedef int	ncl_size_t;
typedef unsigned int	ncl_usize_t;
#else
typedef long	ncl_size_t;
typedef unsigned long	ncl_usize_t;
#endif

#endif

