/*
 *      $Id: util.c,v 1.5 1998-09-25 19:01:50 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		util.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 4 11:23:43 MDT 1996
 *
 *	Description:	
 */
#include <sys/param.h>
#include <sys/stat.h>

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <ncarg/ngo/util.h>

/*
 * Function:	NgHashString
 *
 * Description:	
 *		This function is almost an exact copy of hash_pjw from
 *		Symbol.c in ncl except that the number of buckets is
 *		not hard coded.  nbuckets should be a prime number.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
unsigned int
NgHashString
(
	char		*str,
	unsigned int	nbuckets
)
{
	char		*p;
	unsigned	h=0,g;

	for(p=str;*p != '\0';p++){
		h = (h<<4) + (*p);
		if(g = h & 0xf0000000){
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}

	return h % nbuckets;
}

/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Guido van Rossum.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * static char sccsid[] = "@(#)glob.c	8.3 (Berkeley) 10/13/93";
 * static char rcsid[] = "$NetBSD: glob.c,v 1.5 1995/02/27 04:13:35 cgd Exp $";
 */

/*
 * Ngglob(3) -- a superset of the one defined in POSIX 1003.2.
 *
 * The [!...] convention to negate a range is supported (SysV, Posix, ksh).
 *
 * Optional extra services, controlled by flags not defined by POSIX:
 *
 * NgGLOB_QUOTE:
 *	Escaping convention: \ inhibits any special meaning the following
 *	character might have (except \ at end of string is retained).
 * NgGLOB_MAGCHAR:
 *	Set in ngl_flags if pattern contained a globbing character.
 * NgGLOB_NOMAGIC:
 *	Same as NgGLOB_NOCHECK, but it will only append pattern if it did
 *	not contain any magic characters.  [Used in csh style globbing]
 * NgGLOB_ALTDIRFUNC:
 *	Use alternately specified directory access functions.
 * NgGLOB_TILDE:
 *	expand ~user/foo to the /home/dir/of/user/foo
 * NgGLOB_BRACE:
 *	expand {1,2}{a,b} to 1a 1b 2a 2b 
 * ngl_matchc:
 *	Number of matches in the current invocation of glob.
 */

#define	DOLLAR		'$'
#define	DOT		'.'
#define	EOS		'\0'
#define	LBRACKET	'['
#define	NOT		'!'
#define	QUESTION	'?'
#define	QUOTE		'\\'
#define	RANGE		'-'
#define	RBRACKET	']'
#define	SEP		'/'
#define	STAR		'*'
#define	TILDE		'~'
#define	UNDERSCORE	'_'
#define	LBRACE		'{'
#define	RBRACE		'}'
#define	SLASH		'/'
#define	COMMA		','

#define	M_QUOTE		0x8000
#define	M_PROTECT	0x4000
#define	M_MASK		0xffff
#define	M_ASCII		0x00ff

#ifdef	Linux
typedef unsigned short ushort;
typedef unsigned char unchar;
typedef unsigned int uint;
typedef ushort	ushort_t;
typedef unchar	uchar_t;
typedef uint	uint_t;
#endif

typedef ushort_t NgGlobChar;

#define	CHAR(c)		((NgGlobChar)((c)&M_ASCII))
#define	META(c)		((NgGlobChar)((c)|M_QUOTE))
#define	M_ALL		META('*')
#define	M_END		META(']')
#define	M_NOT		META('!')
#define	M_ONE		META('?')
#define	M_RNG		META('-')
#define	M_SET		META('[')
#define	ismeta(c)	(((c)&M_QUOTE) != 0)


static int compare(
	const void *,
	const void *);
static void g_Ctoc(
	const NgGlobChar *,
	char *);
static int g_lstat(
	NgGlobChar *,
	struct stat *,
	nglob_t *);
static DIR *g_opendir(
	NgGlobChar *,
	nglob_t *);
static NgGlobChar *g_strchr(
	NgGlobChar *,
	int);
static int g_stat(
	NgGlobChar *,
	struct stat *,
	nglob_t *);
static int glob0(
	const NgGlobChar *,
	nglob_t *);
static int glob1(
	NgGlobChar *,
	nglob_t *);
static int glob2(
	NgGlobChar *,
	NgGlobChar *,
	NgGlobChar *,
	nglob_t *);
static int glob3(
	NgGlobChar *,
	NgGlobChar *,
	NgGlobChar *,
	NgGlobChar *,
	nglob_t *);
static int globextend(
	const NgGlobChar *,
	nglob_t *);
static const NgGlobChar * globtilde(
	const NgGlobChar *,
	NgGlobChar *,
	nglob_t *);
static int globexp1(
	const NgGlobChar *,
	nglob_t *);
static int globexp2(
	const NgGlobChar *,
	const NgGlobChar *,
	nglob_t *,
	int *);
static int match(
	NgGlobChar *,
	NgGlobChar *,
	NgGlobChar *);
#ifdef GLOBDEBUG
static void qprintf(
	const char *,
	NgGlobChar *);
#endif

int
Ngglob(pattern, flags, errfunc, pglob)
	const char *pattern;
	int flags, (*errfunc) (const char *, int);
	nglob_t *pglob;
{
	const uchar_t *patnext;
	int c;
	NgGlobChar *bufnext, *bufend, patbuf[MAXPATHLEN+1];

	patnext = (uchar_t *) pattern;
	if (!(flags & NgGLOB_APPEND)) {
		pglob->ngl_pathc = 0;
		pglob->ngl_pathv = NULL;
		if (!(flags & NgGLOB_DOOFFS))
			pglob->ngl_offs = 0;
	}
	pglob->ngl_flags = flags & ~NgGLOB_MAGCHAR;
	pglob->ngl_errfunc = errfunc;
	pglob->ngl_matchc = 0;

	bufnext = patbuf;
	bufend = bufnext + MAXPATHLEN;
	if (flags & NgGLOB_QUOTE) {
		/* Protect the quoted characters. */
		while (bufnext < bufend && (c = *patnext++) != EOS) 
			if (c == QUOTE) {
				if ((c = *patnext++) == EOS) {
					c = QUOTE;
					--patnext;
				}
				*bufnext++ = c | M_PROTECT;
			}
			else
				*bufnext++ = c;
	}
	else 
	    while (bufnext < bufend && (c = *patnext++) != EOS) 
		    *bufnext++ = c;
	*bufnext = EOS;

	if (flags & NgGLOB_BRACE)
	    return globexp1(patbuf, pglob);
	else
	    return glob0(patbuf, pglob);
}

/*
 * Expand recursively a glob {} pattern. When there is no more expansion
 * invoke the standard globbing routine to glob the rest of the magic
 * characters
 */
static int globexp1(pattern, pglob)
	const NgGlobChar *pattern;
	nglob_t *pglob;
{
	const NgGlobChar* ptr = pattern;
	int rv;

	/* Protect a single {}, for find(1), like csh */
	if (pattern[0] == LBRACE && pattern[1] == RBRACE && pattern[2] == EOS)
		return glob0(pattern, pglob);

	while ((ptr = (const NgGlobChar *) g_strchr((NgGlobChar *) ptr, LBRACE)) != NULL)
		if (!globexp2(ptr, pattern, pglob, &rv))
			return rv;

	return glob0(pattern, pglob);
}


/*
 * Recursive brace globbing helper. Tries to expand a single brace.
 * If it succeeds then it invokes globexp1 with the new pattern.
 * If it fails then it tries to glob the rest of the pattern and returns.
 */
static int globexp2(ptr, pattern, pglob, rv)
	const NgGlobChar *ptr, *pattern;
	nglob_t *pglob;
	int *rv;
{
	int     i;
	NgGlobChar   *lm, *ls;
	const NgGlobChar *pe, *pm, *pl;
	NgGlobChar    patbuf[MAXPATHLEN + 1];

	/* copy part up to the brace */
	for (lm = patbuf, pm = pattern; pm != ptr; *lm++ = *pm++)
		continue;
	ls = lm;

	/* Find the balanced brace */
	for (i = 0, pe = ++ptr; *pe; pe++)
		if (*pe == LBRACKET) {
			/* Ignore everything between [] */
			for (pm = pe++; *pe != RBRACKET && *pe != EOS; pe++)
				continue;
			if (*pe == EOS) {
				/* 
				 * We could not find a matching RBRACKET.
				 * Ignore and just look for RBRACE
				 */
				pe = pm;
			}
		}
		else if (*pe == LBRACE)
			i++;
		else if (*pe == RBRACE) {
			if (i == 0)
				break;
			i--;
		}

	/* Non matching braces; just glob the pattern */
	if (i != 0 || *pe == EOS) {
		*rv = glob0(patbuf, pglob);
		return 0;
	}

	for (i = 0, pl = pm = ptr; pm <= pe; pm++)
		switch (*pm) {
		case LBRACKET:
			/* Ignore everything between [] */
			for (pl = pm++; *pm != RBRACKET && *pm != EOS; pm++)
				continue;
			if (*pm == EOS) {
				/* 
				 * We could not find a matching RBRACKET.
				 * Ignore and just look for RBRACE
				 */
				pm = pl;
			}
			break;

		case LBRACE:
			i++;
			break;

		case RBRACE:
			if (i) {
			    i--;
			    break;
			}
			/* FALLTHROUGH */
		case COMMA:
			if (i && *pm == COMMA)
				break;
			else {
				/* Append the current string */
				for (lm = ls; (pl < pm); *lm++ = *pl++)
					continue;
				/* 
				 * Append the rest of the pattern after the
				 * closing brace
				 */
				for (pl = pe + 1; (*lm++ = *pl++) != EOS;)
					continue;

				/* Expand the current pattern */
#ifdef GLOBDEBUG
				qprintf("globexp2:", patbuf);
#endif
				*rv = globexp1(patbuf, pglob);

				/* move after the comma, to the next string */
				pl = pm + 1;
			}
			break;

		default:
			break;
		}
	*rv = 0;
	return 0;
}



/*
 * expand tilde from the passwd file.
 */
static const NgGlobChar *
globtilde(pattern, patbuf, pglob)
	const NgGlobChar *pattern;
	NgGlobChar *patbuf;
	nglob_t *pglob;
{
	struct passwd *pwd;
	char *h;
	const NgGlobChar *p;
	NgGlobChar *b;

	if (*pattern != TILDE || !(pglob->ngl_flags & NgGLOB_TILDE))
		return pattern;

	/* Copy up to the end of the string or / */
	for (p = pattern + 1, h = (char *) patbuf; *p && *p != SLASH; 
	     *h++ = *p++)
		continue;

	*h = EOS;

	if (((char *) patbuf)[0] == EOS) {
		/* 
		 * handle a plain ~ or ~/ by expanding $HOME 
		 * first and then trying the password file
		 */
		if ((h = getenv("HOME")) == NULL) {
			if ((pwd = getpwuid(getuid())) == NULL)
				return pattern;
			else
				h = pwd->pw_dir;
		}
	}
	else {
		/*
		 * Expand a ~user
		 */
		if ((pwd = getpwnam((char*) patbuf)) == NULL)
			return pattern;
		else
			h = pwd->pw_dir;
	}

	/* Copy the home directory */
	for (b = patbuf; *h; *b++ = *h++)
		continue;
	
	/* Append the rest of the pattern */
	while ((*b++ = *p++) != EOS)
		continue;

	return patbuf;
}
	

/*
 * The main glob() routine: compiles the pattern (optionally processing
 * quotes), calls glob1() to do the real pattern matching, and finally
 * sorts the list (unless unsorted operation is requested).  Returns 0
 * if things went well, nonzero if errors occurred.  It is not an error
 * to find no matches.
 */
static int
glob0(pattern, pglob)
	const NgGlobChar *pattern;
	nglob_t *pglob;
{
	const NgGlobChar *qpatnext;
	int c, err, oldpathc;
	NgGlobChar *bufnext, patbuf[MAXPATHLEN+1];

	qpatnext = globtilde(pattern, patbuf, pglob);
	oldpathc = pglob->ngl_pathc;
	bufnext = patbuf;

	/* We don't need to check for buffer overflow any more. */
	while ((c = *qpatnext++) != EOS) {
		switch (c) {
		case LBRACKET:
			c = *qpatnext;
			if (c == NOT)
				++qpatnext;
			if (*qpatnext == EOS ||
			    g_strchr((NgGlobChar *) qpatnext+1, RBRACKET) == NULL) {
				*bufnext++ = LBRACKET;
				if (c == NOT)
					--qpatnext;
				break;
			}
			*bufnext++ = M_SET;
			if (c == NOT)
				*bufnext++ = M_NOT;
			c = *qpatnext++;
			do {
				*bufnext++ = CHAR(c);
				if (*qpatnext == RANGE &&
				    (c = qpatnext[1]) != RBRACKET) {
					*bufnext++ = M_RNG;
					*bufnext++ = CHAR(c);
					qpatnext += 2;
				}
			} while ((c = *qpatnext++) != RBRACKET);
			pglob->ngl_flags |= NgGLOB_MAGCHAR;
			*bufnext++ = M_END;
			break;
		case QUESTION:
			pglob->ngl_flags |= NgGLOB_MAGCHAR;
			*bufnext++ = M_ONE;
			break;
		case STAR:
			pglob->ngl_flags |= NgGLOB_MAGCHAR;
			/* collapse adjacent stars to one, 
			 * to avoid exponential behavior
			 */
			if (bufnext == patbuf || bufnext[-1] != M_ALL)
			    *bufnext++ = M_ALL;
			break;
		default:
			*bufnext++ = CHAR(c);
			break;
		}
	}
	*bufnext = EOS;
#ifdef GLOBDEBUG
	qprintf("glob0:", patbuf);
#endif

	if ((err = glob1(patbuf, pglob)) != 0)
		return(err);

	/*
	 * If there was no match we are going to append the pattern 
	 * if NgGLOB_NOCHECK was specified or if NgGLOB_NOMAGIC was specified
	 * and the pattern did not contain any magic characters
	 * NgGLOB_NOMAGIC is there just for compatibility with csh.
	 */
	if (pglob->ngl_pathc == oldpathc && 
	    ((pglob->ngl_flags & NgGLOB_NOCHECK) || 
	      ((pglob->ngl_flags & NgGLOB_NOMAGIC) &&
	       !(pglob->ngl_flags & NgGLOB_MAGCHAR))))
		return(globextend(pattern, pglob));
	else if (!(pglob->ngl_flags & NgGLOB_NOSORT)) 
		qsort(pglob->ngl_pathv + pglob->ngl_offs + oldpathc,
		    pglob->ngl_pathc - oldpathc, sizeof(char *), compare);
	return(0);
}

static int
compare(p, q)
	const void *p, *q;
{
	return(strcmp(*(char **)p, *(char **)q));
}

static int
glob1(pattern, pglob)
	NgGlobChar *pattern;
	nglob_t *pglob;
{
	NgGlobChar pathbuf[MAXPATHLEN+1];

	/* A null pathname is invalid -- POSIX 1003.1 sect. 2.4. */
	if (*pattern == EOS)
		return(0);
	return(glob2(pathbuf, pathbuf, pattern, pglob));
}

/*
 * The functions glob2 and glob3 are mutually recursive; there is one level
 * of recursion for each segment in the pattern that contains one or more
 * meta characters.
 */
static int
glob2(pathbuf, pathend, pattern, pglob)
	NgGlobChar *pathbuf, *pathend, *pattern;
	nglob_t *pglob;
{
	struct stat sb;
	NgGlobChar *p, *q;
	int anymeta;

	/*
	 * Loop over pattern segments until end of pattern or until
	 * segment with meta character found.
	 */
	for (anymeta = 0;;) {
		if (*pattern == EOS) {		/* End of pattern? */
			*pathend = EOS;
			if (g_lstat(pathbuf, &sb, pglob))
				return(0);
		
			if (((pglob->ngl_flags & NgGLOB_MARK) &&
			    pathend[-1] != SEP) && (S_ISDIR(sb.st_mode)
#ifdef Linux
			    || (__S_ISTYPE((sb.st_mode), __S_IFLNK) &&
#else
			    || (S_ISLNK(sb.st_mode) &&
#endif
			    (g_stat(pathbuf, &sb, pglob) == 0) &&
			    S_ISDIR(sb.st_mode)))) {
				*pathend++ = SEP;
				*pathend = EOS;
			}
			++pglob->ngl_matchc;
			return(globextend(pathbuf, pglob));
		}

		/* Find end of next segment, copy tentatively to pathend. */
		q = pathend;
		p = pattern;
		while (*p != EOS && *p != SEP) {
			if (ismeta(*p))
				anymeta = 1;
			*q++ = *p++;
		}

		if (!anymeta) {		/* No expansion, do next segment. */
			pathend = q;
			pattern = p;
			while (*pattern == SEP)
				*pathend++ = *pattern++;
		} else			/* Need expansion, recurse. */
			return(glob3(pathbuf, pathend, pattern, p, pglob));
	}
	/* NOTREACHED */
}

static int
glob3(pathbuf, pathend, pattern, restpattern, pglob)
	NgGlobChar *pathbuf, *pathend, *pattern, *restpattern;
	nglob_t *pglob;
{
	register struct dirent *dp;
	DIR *dirp;
	int err;
	char buf[MAXPATHLEN];

	/*
	 * The readdirfunc declaration can't be prototyped, because it is
	 * assigned, below, to two functions which are prototyped in glob.h
	 * and dirent.h as taking pointers to differently typed opaque
	 * structures.
	 */
	struct dirent *(*readdirfunc)();

	*pathend = EOS;
	errno = 0;
	    
	if ((dirp = g_opendir(pathbuf, pglob)) == NULL) {
		/* TODO: don't call for ENOENT or ENOTDIR? */
		if (pglob->ngl_errfunc) {
			g_Ctoc(pathbuf, buf);
			if (pglob->ngl_errfunc(buf, errno) ||
			    pglob->ngl_flags & NgGLOB_ERR)
				return (NgGLOB_ABEND);
		}
		return(0);
	}

	err = 0;

	/* Search directory for matching names. */
	if (pglob->ngl_flags & NgGLOB_ALTDIRFUNC)
		readdirfunc = pglob->ngl_readdir;
	else
		readdirfunc = readdir;
	while ((dp = (*readdirfunc)(dirp))) {
		register uchar_t *sc;
		register NgGlobChar *dc;

		/* Initial DOT must be matched literally. */
		if (dp->d_name[0] == DOT && *pattern != DOT)
			continue;
		for (sc = (uchar_t *) dp->d_name, dc = pathend; 
		     (*dc++ = *sc++) != EOS;)
			continue;
		if (!match(pathend, pattern, restpattern)) {
			*pathend = EOS;
			continue;
		}
		err = glob2(pathbuf, --dc, restpattern, pglob);
		if (err)
			break;
	}

	if (pglob->ngl_flags & NgGLOB_ALTDIRFUNC)
		(*pglob->ngl_closedir)(dirp);
	else
		closedir(dirp);
	return(err);
}


/*
 * Extend the ngl_pathv member of a nglob_t structure to accomodate a new item,
 * add the new item, and update ngl_pathc.
 *
 * This assumes the BSD realloc, which only copies the block when its size
 * crosses a power-of-two boundary; for v7 realloc, this would cause quadratic
 * behavior.
 *
 * Return 0 if new item added, error code if memory couldn't be allocated.
 *
 * Invariant of the nglob_t structure:
 *	Either ngl_pathc is zero and ngl_pathv is NULL; or ngl_pathc > 0 and
 *	ngl_pathv points to (ngl_offs + ngl_pathc + 1) items.
 */
static int
globextend(path, pglob)
	const NgGlobChar *path;
	nglob_t *pglob;
{
	register char **pathv;
	register int i;
	uint_t newsize;
	char *copy;
	const NgGlobChar *p;

	newsize = sizeof(*pathv) * (2 + pglob->ngl_pathc + pglob->ngl_offs);
	pathv = pglob->ngl_pathv ? 
		    realloc((char *)pglob->ngl_pathv, newsize) :
		    malloc(newsize);
	if (pathv == NULL)
		return(NgGLOB_NOSPACE);

	if (pglob->ngl_pathv == NULL && pglob->ngl_offs > 0) {
		/* first time around -- clear initial ngl_offs items */
		pathv += pglob->ngl_offs;
		for (i = pglob->ngl_offs; --i >= 0; )
			*--pathv = NULL;
	}
	pglob->ngl_pathv = pathv;

	for (p = path; *p++;)
		continue;
	if ((copy = malloc(p - path)) != NULL) {
		g_Ctoc(path, copy);
		pathv[pglob->ngl_offs + pglob->ngl_pathc++] = copy;
	}
	pathv[pglob->ngl_offs + pglob->ngl_pathc] = NULL;
	return(copy == NULL ? NgGLOB_NOSPACE : 0);
}


/*
 * pattern matching function for filenames.  Each occurrence of the *
 * pattern causes a recursion level.
 */
static int
match(name, pat, patend)
	register NgGlobChar *name, *pat, *patend;
{
	int ok, negate_range;
	NgGlobChar c, k;

	while (pat < patend) {
		c = *pat++;
		switch (c & M_MASK) {
		case M_ALL:
			if (pat == patend)
				return(1);
			do 
			    if (match(name, pat, patend))
				    return(1);
			while (*name++ != EOS);
			return(0);
		case M_ONE:
			if (*name++ == EOS)
				return(0);
			break;
		case M_SET:
			ok = 0;
			if ((k = *name++) == EOS)
				return(0);
			if ((negate_range = ((*pat & M_MASK) == M_NOT)) != EOS)
				++pat;
			while (((c = *pat++) & M_MASK) != M_END)
				if ((*pat & M_MASK) == M_RNG) {
					if (c <= k && k <= pat[1])
						ok = 1;
					pat += 2;
				} else if (c == k)
					ok = 1;
			if (ok == negate_range)
				return(0);
			break;
		default:
			if (*name++ != c)
				return(0);
			break;
		}
	}
	return(*name == EOS);
}

/* Free allocated data belonging to a nglob_t structure. */
void
Ngglobfree(pglob)
	nglob_t *pglob;
{
	register int i;
	register char **pp;

	if (pglob->ngl_pathv != NULL) {
		pp = pglob->ngl_pathv + pglob->ngl_offs;
		for (i = pglob->ngl_pathc; i--; ++pp)
			if (*pp)
				free(*pp);
		free(pglob->ngl_pathv);
	}
}

static DIR *
g_opendir(str, pglob)
	register NgGlobChar *str;
	nglob_t *pglob;
{
	char buf[MAXPATHLEN];

	if (!*str)
		strcpy(buf, ".");
	else
		g_Ctoc(str, buf);

	if (pglob->ngl_flags & NgGLOB_ALTDIRFUNC)
		return((*pglob->ngl_opendir)(buf));

	return(opendir(buf));
}

static int
g_lstat(fn, sb, pglob)
	register NgGlobChar *fn;
	struct stat *sb;
	nglob_t *pglob;
{
	char buf[MAXPATHLEN];

	g_Ctoc(fn, buf);
	if (pglob->ngl_flags & NgGLOB_ALTDIRFUNC)
		return((*pglob->ngl_lstat)(buf, sb));
	return(lstat(buf, sb));
}

static int
g_stat(fn, sb, pglob)
	register NgGlobChar *fn;
	struct stat *sb;
	nglob_t *pglob;
{
	char buf[MAXPATHLEN];

	g_Ctoc(fn, buf);
	if (pglob->ngl_flags & NgGLOB_ALTDIRFUNC)
		return((*pglob->ngl_stat)(buf, sb));
	return(stat(buf, sb));
}

static NgGlobChar *
g_strchr(str, ch)
	NgGlobChar *str;
	int ch;
{
	do {
		if (*str == ch)
			return (str);
	} while (*str++);
	return (NULL);
}

static void
g_Ctoc(str, buf)
	register const NgGlobChar *str;
	char *buf;
{
	register char *dc;

	for (dc = buf; (*dc++ = *str++) != EOS;)
		continue;
}

#ifdef GLOBDEBUG
static void 
qprintf(str, s)
	const char *str;
	register NgGlobChar *s;
{
	register NgGlobChar *p;

	(void)printf("%s:\n", str);
	for (p = s; *p; p++)
		(void)printf("%c", CHAR(*p));
	(void)printf("\n");
	for (p = s; *p; p++)
		(void)printf("%c", *p & M_PROTECT ? '"' : ' ');
	(void)printf("\n");
	for (p = s; *p; p++)
		(void)printf("%c", ismeta(*p) ? '_' : ' ');
	(void)printf("\n");
}
#endif
