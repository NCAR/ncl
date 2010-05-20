#ifndef _NGSIZET_H
#define _NGSIZET_H

#ifdef NG32BIT
typedef int		ng_size_t;
typedef unsigned int	ng_usize_t;
#else
typedef long		ng_size_t;
typedef unsigned long	ng_usize_t;
#endif

#endif

