/*
 * sdbm - ndbm work-alike hashed database library
 * based on Per-Aake Larson's Dynamic Hashing algorithms.
 * BIT 18 (1978).
 *
 * Copyright (c) 1991 by Ozan S. Yigit (oz@nexus.yorku.ca)
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation.
 *
 * This file is provided AS IS with no warranties of any kind.  The author
 * shall have no liability with respect to the infringement of copyrights,
 * trade secrets or any patents by this file or any part thereof.  In no
 * event will the author be liable for any lost revenue or profits or
 * other special, indirect and consequential damages.
 */

#define DBLKSIZ 4096
#define PBLKSIZ 1024
#define PAIRMAX 1008			/* arbitrary on PBLKSIZ-N */
#define SPLTMAX	10			/* maximum allowed splits */
					/* for a single insertion */
#define DIRFEXT	".dir"
#define PAGFEXT	".pag"

typedef struct {
	int dirf;		       /* directory file descriptor */
	int pagf;		       /* page file descriptor */
	int flags;		       /* status/error flags, see below */
	long maxbno;		       /* size of dirfile in bits */
	long curbit;		       /* current bit number */
	long hmask;		       /* current hash mask */
	long blkptr;		       /* current block for nextkey */
	int keyptr;		       /* current key for nextkey */
	long blkno;		       /* current page to read/write */
	long pagbno;		       /* current page in pagbuf */
	char pagbuf[PBLKSIZ];	       /* page file block buffer */
	long dirbno;		       /* current block in dirbuf */
	char dirbuf[DBLKSIZ];	       /* directory file block buffer */
} DBM;

#define DBM_RDONLY	0x1	       /* data base open read-only */
#define DBM_IOERR	0x2	       /* data base I/O error */

/*
 * utility macros
 */
#define dbm_rdonly(db)		((db)->flags & DBM_RDONLY)
#define dbm_error(db)		((db)->flags & DBM_IOERR)

#define dbm_clearerr(db)	((db)->flags &= ~DBM_IOERR)  /* ouch */

#define dbm_dirfno(db)	((db)->dirf)
#define dbm_pagfno(db)	((db)->pagf)

typedef struct {
	char *dptr;
	int dsize;
} datum;

extern datum nullitem;

#ifdef __STDC__
#define proto(p) p
#else
#define proto(p) ()
#endif

/*
 * flags to dbm_store
 */
#define DBM_INSERT	0
#define DBM_REPLACE	1

/*
 * ndbm interface
 */
extern DBM *dbm_open proto((char *, int, int));
extern void dbm_close proto((DBM *));
extern datum dbm_fetch proto((DBM *, datum));
extern int dbm_delete proto((DBM *, datum));
extern int dbm_store proto((DBM *, datum, datum, int));
extern datum dbm_firstkey proto((DBM *));
extern datum dbm_nextkey proto((DBM *));

/*
 * other
 */
extern DBM *dbm_prep proto((char *, char *, int, int));
extern long dbm_hash proto((char *, int));
