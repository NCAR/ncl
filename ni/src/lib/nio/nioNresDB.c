/*
 *      $Id: nioNresDB.c,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		NresDB.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Dec 14 14:39:40 MST 1992
 *
 *	Description:	This file was taken from the mit X distribution and
 *			modified to support resource management for the
 *			hlu's.  I started with the X code and modified it
 *			as I needed to - therefore the following X copyright
 *			notice is here.  The above copyright should be taken
 *			to copyright all changes to the code below.
 */

/***********************************************************
Copyright 1987, 1988, 1990 by Digital Equipment Corporation, Maynard,
Massachusetts, and the Massachusetts Institute of Technology, Cambridge,
Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include	<stdio.h>
#include	<unistd.h>
#include	<ctype.h>

#include	"niohluP.h"
#include	"nioNresDB.h"
#include	"nioConvert.h"

#if	NhlNeedProto && !defined(VMS)
#define RConst const
#else
#define RConst /**/
#endif

/*

These Xrm routines allow very fast lookup of resources in the resource
database.  Several usage patterns are exploited:

(1) Widgets get a lot of resources at one time.  Rather than look up each from
scratch, we can precompute the prioritized list of database levels once, then
search for each resource starting at the beginning of the list.

(2) Many database levels don't contain any leaf resource nodes.  There is no
point in looking for resources on a level that doesn't contain any.  This
information is kept on a per-level basis.

(3) Sometimes the widget instance tree is structured such that you get the same
class name repeated on the fully qualified widget name.  This can result in the
same database level occuring multiple times on the search list.  The code below
only checks to see if you get two identical search lists in a row, rather than
look back through all database levels, but in practice this removes all
duplicates I've ever observed.

Joel McCormack

*/

/*

The Xrm representation has been completely redesigned to substantially reduce
memory and hopefully improve performance.

The database is structured into two kinds of tables: LTables that contain
only values, and NTables that contain only other tables.

Some invariants:

The next pointer of the top-level node table points to the top-level leaf
table, if any.

Within an LTable, for a given name, the tight value always precedes the
loose value, and if both are present the loose value is always right after
the tight value.

Within an NTable, all of the entries for a given name are contiguous,
in the order tight NTable, loose NTable, tight LTable, loose LTable.

Bob Scheifler

*/

static NrmQuark NrmQString, NrmQANY;

typedef struct _VEntry {
    struct _VEntry	*next;		/* next in chain */
    NrmQuark		name;		/* name of this entry */
    unsigned int	tight:1;	/* 1 if it is a tight binding */
    unsigned int	string:1;	/* 1 if type is String */
    unsigned int	size:30;	/* size of value */
} VEntryRec, *VEntry;


typedef struct _DEntry {
    VEntryRec		entry;		/* entry */
    NrmRepresentation	type;		/* representation type */
} DEntryRec, *DEntry;

/* the value is right after the structure */
#define StringValue(ve) (NhlPointer)((ve) + 1)
#define RepType(ve) ((DEntry)(ve))->type
/* the value is right after the structure */
#define DataValue(ve) (NhlPointer)(((DEntry)(ve)) + 1)
#define RawValue(ve) (char *)((ve)->string ? StringValue(ve) : DataValue(ve))

typedef struct _NTable {
    struct _NTable	*next;		/* next in chain */
    NrmQuark		name;		/* name of this entry */
    unsigned int	tight:1;	/* 1 if it is a tight binding */
    unsigned int	leaf:1;		/* 1 if children are values */
    unsigned int	hasloose:1;	/* 1 if has loose children */
    unsigned int	hasany:1;	/* 1 if has ANY entry */
    unsigned int	pad:4;		/* unused */
    unsigned int	mask:8;		/* hash size - 1 */
    unsigned int	entries:16;	/* number of children */
} NTableRec, *NTable;

/* the buckets are right after the structure */
#define NodeBuckets(ne) ((NTable *)((ne) + 1))
#define NodeHash(ne,q) NodeBuckets(ne)[(q) & (ne)->mask]

/* leaf tables have an extra level of indirection for the buckets,
 * so that resizing can be done without invalidating a search list.
 * This is completely ugly, and wastes some memory, but the Nlib
 * spec doesn't really specify whether invalidation is OK, and the
 * old implementation did not invalidate.
 */
typedef struct _LTable {
    NTableRec		table;
    VEntry		*buckets;
} LTableRec, *LTable;

#define LeafHash(le,q) (le)->buckets[(q) & (le)->table.mask]

typedef struct { 
	void (*mbinit)( 
#if		NhlNeedProto
		NhlPointer        /* state */ 
#endif 
	); 
	char (*mbchar)( 
#if		NhlNeedProto 
		NhlPointer        /* state */, 
		char*           /* str */, 
		int*            /* lenp */ 
#endif 
	); 
	void (*mbfinish)( 
#if		NhlNeedProto 
		NhlPointer /* state */ 
#endif 
	); 
	char* (*lcname)( 
#if		NhlNeedProto 
		NhlPointer /* state */ 
#endif 
	); 
	void (*destroy)( 
#if		NhlNeedProto 
		NhlPointer /* state */ 
#endif 
	); 
} NrmMethodsRec, *NrmMethods;

/* An NrmDatabase just holds a pointer to the first top-level table.
 * The type name is no longer descriptive, but better to not change
 * the Nresource.h header file.  This type also gets used to define
 * NrmSearchList, which is a complete crock, but we'll just leave it
 * and caste types as required.
 */
typedef struct _NrmHashBucketRec {
    NTable table;
    NhlPointer mbstate;
    NrmMethods methods;
} NrmHashBucketRec;

/* closure used in get/put resource */
typedef struct _VClosure {
    NrmRepresentation	*type;		/* type of value */
    NrmValuePtr		value;		/* value itself */
} VClosureRec, *VClosure;

/* closure used in get search list */
typedef struct _SClosure {
    LTable		*list;		/* search list */
    int			idx;		/* index of last filled element */
    int			limit;		/* maximum index */
} SClosureRec, *SClosure;

/* placed in NrmSearchList to indicate next table is loose only */
#define LOOSESEARCH ((LTable)1)

/* closure used in enumerate database */
typedef struct _EClosure {
    NrmDatabase db;			/* the database */
    NrmDBEnumProc proc;			/* the user proc */
    NhlPointer closure;			/* the user closure */
    NrmBindingList bindings;		/* binding list */
    NrmQuarkList quarks;		/* quark list */
    int mode;				/* NrmEnum<kind> */
} EClosureRec, *EClosure;

/* predicate to determine when to resize a hash table */
#define GrowthPred(n,m) ((unsigned)(n) > (((m) + 1) << 2))

#define GROW(prev) \
    if (GrowthPred((*prev)->entries, (*prev)->mask)) \
	GrowTable(prev)

/* pick a reasonable value for maximum depth of resource database */
#define MANDBDEPTH 100

/* macro used in get/search functions */

/* find an entry named ename, with leafness given by leaf */
#define NFIND(ename) \
    q = ename; \
    entry = NodeHash(table, q); \
    while (entry && entry->name != q) \
	entry = entry->next; \
    if (leaf && entry && !entry->leaf) { \
	entry = entry->next; \
	if (entry && !entry->leaf) \
	    entry = entry->next; \
	if (entry && entry->name != q) \
	    entry = (NTable)NULL; \
    }

/* resourceQuarks keeps track of what quarks have been associated with values
 * in all LTables.  If a quark has never been used in an LTable, we don't need
 * to bother looking for it.
 */

static unsigned char *resourceQuarks = (unsigned char *)NULL;
static NrmQuark maxResourceQuark = -1;

/* determines if a quark has been used for a value in any database */
#define IsResourceQuark(q)  ((q) > 0 && (q) <= maxResourceQuark && \
			     resourceQuarks[(q) >> 3] & (1 << ((q) & 7)))

typedef unsigned char NrmBits;

#define BSLASH  ((NrmBits) (1 << 5))
#define NORMAL	((NrmBits) (1 << 4))
#define EOQ	((NrmBits) (1 << 3))
#define SEP	((NrmBits) (1 << 2))
#define ENDOF	((NrmBits) (1 << 1))
#define SPACE	(NORMAL|EOQ|SEP|(NrmBits)0)
#define RSEP	(NORMAL|EOQ|SEP|(NrmBits)1)
#define EOS	(EOQ|SEP|ENDOF|(NrmBits)0)
#define EOL	(EOQ|SEP|ENDOF|(NrmBits)1)
#define BINDING	(NORMAL|EOQ)
#define ODIGIT	(NORMAL|(NrmBits)1)

#define next_char(ch,str) xrmtypes[(unsigned char)((ch) = *(++(str)))]
#define next_mbchar(ch,len,str) xrmtypes[(unsigned char)(ch = (*db->methods->mbchar)(db->mbstate, str, &len), str += len, ch)]

#define is_space(bits)		((bits) == SPACE)
#define is_EOQ(bits)		((bits) & EOQ)
#define is_EOF(bits)		((bits) == EOS)
#define is_EOL(bits)		((bits) & ENDOF)
#define is_binding(bits)	((bits) == BINDING)
#define is_odigit(bits)		((bits) == ODIGIT)
#define is_separator(bits)	((bits) & SEP)
#define is_nonpcs(bits)		(!(bits))
#define is_normal(bits)		((bits) & NORMAL)
#define is_simple(bits)		((bits) & (NORMAL|BSLASH))
#define is_special(bits)	((bits) & (ENDOF|BSLASH))

/* parsing types */
static NrmBits Const xrmtypes[256] = {
    EOS,0,0,0,0,0,0,0,
    0,SPACE,EOL,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    SPACE,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,
    NORMAL,NORMAL,BINDING,NORMAL,NORMAL,NORMAL,BINDING,NORMAL,
    ODIGIT,ODIGIT,ODIGIT,ODIGIT,ODIGIT,ODIGIT,ODIGIT,ODIGIT,
    NORMAL,NORMAL,RSEP,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,
    NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,
    NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,
    NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,
    NORMAL,NORMAL,NORMAL,NORMAL,BSLASH,NORMAL,NORMAL,NORMAL,
    NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,
    NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,
    NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,
    NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,NORMAL,0
    /* The rest will be automatically initialized to zero. */
};

void
_NrmInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
    NrmQString = NrmPermStringToQuark("String");
    NrmQANY = NrmPermStringToQuark("?");
}

#if NhlNeedProto
void NrmStringToQuarkList(
    register Const char  *name,
    register NrmQuarkList quarks)   /* RETURN */
#else
void NrmStringToQuarkList(name, quarks)
    register Const char	 *name;
    register NrmQuarkList quarks;   /* RETURN */
#endif
{
    register NrmBits		bits;
    register Signature  	sig = 0;
    register char       	ch, *tname;
    register int 		i = 0;

    if ((tname = (char *)name) != (char *)NULL) {
	tname--;
	while (!is_EOF(bits = next_char(ch, tname))) {
	    if (is_binding (bits)) {
		if (i) {
		    /* Found a complete name */
		    *quarks++ = _NrmInternalStringToQuark(name,tname - name,
							  sig, False);
		    i = 0;
		    sig = 0;
		}
		name = tname+1;
	    }
	    else {
		sig = (sig << 1) + ch; /* Compute the signature. */
		i++;
	    }
	}
	*quarks++ = _NrmInternalStringToQuark(name, tname - name, sig, False);
    }
    *quarks = NrmNULLQUARK;
}

#if NhlNeedProto
void NrmStringToBindingQuarkList(
    register Const char   *name,
    register NrmBindingList bindings,   /* RETURN */
    register NrmQuarkList   quarks)     /* RETURN */
#else
void NrmStringToBindingQuarkList(name, bindings, quarks)
    register Const char	    *name;
    register NrmBindingList bindings;   /* RETURN */
    register NrmQuarkList   quarks;     /* RETURN */
#endif
{
    register NrmBits		bits;
    register Signature  	sig = 0;
    register char       	ch, *tname;
    register NrmBinding 	binding;
    register int 		i = 0;

    if ((tname = (char *)name) != (char *)NULL) {
	tname--;
	binding = NrmBindTightly;
	while (!is_EOF(bits = next_char(ch, tname))) {
	    if (is_binding (bits)) {
		if (i) {
		    /* Found a complete name */
		    *bindings++ = binding;
		    *quarks++ = _NrmInternalStringToQuark(name, tname - name,
							  sig, False);

		    i = 0;
		    sig = 0;
		    binding = NrmBindTightly;
		}
		name = tname+1;

		if (ch == '*')
		    binding = NrmBindLoosely;
	    }
	    else {
		sig = (sig << 1) + ch; /* Compute the signature. */
		i++;
	    }
	}
	*bindings = binding;
	*quarks++ = _NrmInternalStringToQuark(name, tname - name, sig, False);
    }
    *quarks = NrmNULLQUARK;
}

#ifdef DEBUG

#ifdef OLD_STUFF
static void PrintQuarkList(quarks, stream)
    NrmQuarkList    quarks;
    FILE	    *stream;
{
    NhlBoolean	    firstNameSeen;

    for (firstNameSeen = False; *quarks; quarks++) {
	if (firstNameSeen) {
	    (void) fprintf(stream, ".");
	}
	firstNameSeen = True;
	(void) fputs(NrmQuarkToString(*quarks), stream);
    }
} /* PrintQuarkList */

#endif /* OLD_STUFF */
#endif /* DEBUG */

/*ARGSUSED*/
static void mbnoop(state)
    NhlPointer state;
{
	return;
}

/*ARGSUSED*/
static char mbchar(state, str, lenp)
    NhlPointer state;
    char *str;
    int *lenp;
{
    *lenp = 1;
    return *str;
}

/*ARGSUSED*/
static char *lcname(state)
    NhlPointer state;
{
    return "C";
}

static RConst NrmMethodsRec mb_methods = {
    mbnoop,
    mbchar,
    mbnoop,
    lcname,
    mbnoop
};

static NrmDatabase NewDatabase()
{
    register NrmDatabase db;

    db = (NrmDatabase) NhlMalloc(sizeof(NrmHashBucketRec));
    if (db) {
	db->table = (NTable)NULL;
	/*
	 * This is where you would use a function to set the methods based
	 * on the actual local - I am just making it default to this one
	 */
	db->mbstate = NULL;
	db->methods = (NrmMethods)&mb_methods;
    }
    return db;
}

/* move all values from ftable to ttable, and free ftable's buckets.
 * ttable is quaranteed empty to start with.
 */
static void MoveValues(ftable, ttable)
    LTable ftable;
    register LTable ttable;
{
    register VEntry fentry, nfentry;
    register VEntry *prev;
    register VEntry *bucket;
    register VEntry tentry;
    register int i;

    for (i = ftable->table.mask, bucket = ftable->buckets; i >= 0; i--) {
	for (fentry = *bucket++; fentry; fentry = nfentry) {
	    prev = &LeafHash(ttable, fentry->name);
	    tentry = *prev;
	    *prev = fentry;
	    /* chain on all with same name, to preserve invariant order */
	    while (((nfentry=fentry->next)!=0)&&(nfentry->name == fentry->name))
		fentry = nfentry;
	    fentry->next = tentry;
	}
    }
    NhlFree((char *)ftable->buckets);
}

/* move all tables from ftable to ttable, and free ftable.
 * ttable is quaranteed empty to start with.
 */
static void MoveTables(ftable, ttable)
    NTable ftable;
    register NTable ttable;
{
    register NTable fentry, nfentry;
    register NTable *prev;
    register NTable *bucket;
    register NTable tentry;
    register int i;

    for (i = ftable->mask, bucket = NodeBuckets(ftable); i >= 0; i--) {
	for (fentry = *bucket++; fentry; fentry = nfentry) {
	    prev = &NodeHash(ttable, fentry->name);
	    tentry = *prev;
	    *prev = fentry;
	    /* chain on all with same name, to preserve invariant order */
	    while (((nfentry=fentry->next)!=NULL)&& nfentry->name==fentry->name)
		fentry = nfentry;
	    fentry->next = tentry;
	}
    }
    NhlFree((char *)ftable);
}

/* grow the table, based on current number of entries */
static void GrowTable(prev)
    NTable *prev;
{
    register NTable table;
    register int i;

    table = *prev;
    i = table->mask;
    if (i == 255) /* biggest it gets */
	return;
    while (i < 255 && GrowthPred(table->entries, i))
	i = (i << 1) + 1;
    i++; /* i is now the new size */
    if (table->leaf) {
	register LTable ltable;
	LTableRec otable;

	ltable = (LTable)table;
	/* cons up a copy to make MoveValues look symmetric */
	otable = *ltable;
	ltable->buckets = (VEntry *)NhlMalloc(i * sizeof(VEntry));
	if (!ltable->buckets) {
	    ltable->buckets = otable.buckets;
	    return;
	}
	ltable->table.mask = i - 1;
	memset((char*)ltable->buckets,0, sizeof(VEntry) * i);
	MoveValues(&otable, ltable);
    } else {
	register NTable ntable;

	ntable = (NTable)NhlMalloc(sizeof(NTableRec) + i * sizeof(NTable));
	if (!ntable)
	    return;
	*ntable = *table;
	ntable->mask = i - 1;
	memset((char*)NodeBuckets(ntable),0,sizeof(NTable)*i);
	*prev = ntable;
	MoveTables(table, ntable);
    }
}

/* merge values from ftable into *pprev, destroy ftable in the process */
static void MergeValues(ftable, pprev, override)
    LTable ftable;
    NTable *pprev;
    NhlBoolean override;
{
    register VEntry fentry, tentry;
    register VEntry *prev;
    register LTable ttable;
    VEntry *bucket;
    int i;
    register NrmQuark q;

    ttable = (LTable)*pprev;
    if (ftable->table.hasloose)
	ttable->table.hasloose = 1;
    for (i = ftable->table.mask, bucket = ftable->buckets;
	 i >= 0;
	 i--, bucket++) {
	for (fentry = *bucket; fentry; ) {
	    q = fentry->name;
	    prev = &LeafHash(ttable, q);
	    tentry = *prev;
	    while (tentry && tentry->name != q)
		tentry = *(prev = &tentry->next);
	    /* note: test intentionally uses fentry->name instead of q */
	    /* permits serendipitous inserts */
	    while (tentry && tentry->name == fentry->name) {
		/* if tentry is earlier, skip it */
		if (!fentry->tight && tentry->tight) {
		    tentry = *(prev = &tentry->next);
		    continue;
		}
		if (fentry->tight != tentry->tight) {
		    /* no match, chain in fentry */
		    *prev = fentry;
		    prev = &fentry->next;
		    fentry = *prev;
		    *prev = tentry;
		    ttable->table.entries++;
		} else if (override) {
		    /* match, chain in fentry, splice out and free tentry */
		    *prev = fentry;
		    prev = &fentry->next;
		    fentry = *prev;
		    *prev = tentry->next;
		    /* free the overridden entry */
		    NhlFree((char *)tentry);
		    /* get next tentry */
		    tentry = *prev;
		} else {
		    /* match, discard fentry */
		    prev = &tentry->next;
		    tentry = fentry; /* use as a temp var */
		    fentry = fentry->next;
		    /* free the overpowered entry */
		    NhlFree((char *)tentry);
		    /* get next tentry */
		    tentry = *prev;
		}
		if (!fentry)
		    break;
	    }
	    /* at this point, tentry cannot match any fentry named q */
	    /* chain in all bindings together, preserve invariant order */
	    while (fentry && fentry->name == q) {
		*prev = fentry;
		prev = &fentry->next;
		fentry = *prev;
		*prev = tentry;
		ttable->table.entries++;
	    }
	}
    }
    NhlFree((char *)ftable->buckets);
    NhlFree((char *)ftable);
    /* resize if necessary, now that we're all done */
    GROW(pprev);
}

/* merge tables from ftable into *pprev, destroy ftable in the process */
static void MergeTables(ftable, pprev, override)
    NTable ftable;
    NTable *pprev;
    NhlBoolean override;
{
    register NTable fentry, tentry;
    NTable nfentry;
    register NTable *prev;
    register NTable ttable;
    NTable *bucket;
    int i;
    register NrmQuark q;

    ttable = *pprev;
    if (ftable->hasloose)
	ttable->hasloose = 1;
    if (ftable->hasany)
	ttable->hasany = 1;
    for (i = ftable->mask, bucket = NodeBuckets(ftable);
	 i >= 0;
	 i--, bucket++) {
	for (fentry = *bucket; fentry; ) {
	    q = fentry->name;
	    prev = &NodeHash(ttable, q);
	    tentry = *prev;
	    while (tentry && tentry->name != q)
		tentry = *(prev = &tentry->next);
	    /* note: test intentionally uses fentry->name instead of q */
	    /* permits serendipitous inserts */
	    while (tentry && tentry->name == fentry->name) {
		/* if tentry is earlier, skip it */
		if ((fentry->leaf && !tentry->leaf) ||
		    (!fentry->tight && tentry->tight &&
		     (fentry->leaf || !tentry->leaf))) {
		    tentry = *(prev = &tentry->next);
		    continue;
		}
		nfentry = fentry->next;
		if (fentry->leaf != tentry->leaf ||
		    fentry->tight != tentry->tight) {
		    /* no match, just chain in */
		    *prev = fentry;
		    *(prev = &fentry->next) = tentry;
		    ttable->entries++;
		} else {
		    if (fentry->leaf)
			MergeValues((LTable)fentry, prev, override);
		    else
			MergeTables(fentry, prev, override);
		    /* bump to next tentry */
		    tentry = *(prev = &(*prev)->next);
		}
		/* bump to next fentry */
		fentry = nfentry;
		if (!fentry)
		    break;
	    }
	    /* at this point, tentry cannot match any fentry named q */
	    /* chain in all bindings together, preserve invariant order */
	    while (fentry && fentry->name == q) {
		*prev = fentry;
		prev = &fentry->next;
		fentry = *prev;
		*prev = tentry;
		ttable->entries++;
	    }
	}
    }
    NhlFree((char *)ftable);
    /* resize if necessary, now that we're all done */
    GROW(pprev);
}

void NrmCombineDatabase(from, into, override)
    NrmDatabase	from, *into;
    NhlBoolean override;
{
    register NTable *prev;
    register NTable ftable, ttable, nftable;

    if (!*into) {
	*into = from;
    } else if (from) {
	if ((ftable = from->table) != NULL) {
	    prev = &(*into)->table;
	    ttable = *prev;
	    if (!ftable->leaf) {
		nftable = ftable->next;
		if (ttable && !ttable->leaf) {
		    /* both have node tables, merge them */
		    MergeTables(ftable, prev, override);
		    /* bump to into's leaf table, if any */
		    ttable = *(prev = &(*prev)->next);
		} else {
		    /* into has no node table, link from's in */
		    *prev = ftable;
		    *(prev = &ftable->next) = ttable;
		}
		/* bump to from's leaf table, if any */
		ftable = nftable;
	    } else {
		/* bump to into's leaf table, if any */
		if (ttable && !ttable->leaf)
		    ttable = *(prev = &ttable->next);
	    }
	    if (ftable) {
		/* if into has a leaf, merge, else insert */
		if (ttable)
		    MergeValues((LTable)ftable, prev, override);
		else
		    *prev = ftable;
	    }
	}
	NhlFree((char *)from);
    }
}

void NrmMergeDatabases(from, into)
    NrmDatabase	from, *into;
{
    NrmCombineDatabase(from, into, True);
}

/* store a value in the database, overriding any existing entry */
static void PutEntry(db, bindings, quarks, type, value)
    NrmDatabase		db;
    NrmBindingList	bindings;
    NrmQuarkList	quarks;
    NrmRepresentation	type;
    NrmValuePtr		value;
{
    register NTable *pprev, *prev;
    register NTable table;
    register NrmQuark q;
    register VEntry *vprev;
    register VEntry entry;
    NTable *nprev, *firstpprev;

#define NEWTABLE(q,i) \
    table = (NTable)NhlMalloc(sizeof(LTableRec)); \
    if (!table) \
	return; \
    table->name = q; \
    table->hasloose = 0; \
    table->hasany = 0; \
    table->mask = 0; \
    table->entries = 0; \
    if (quarks[i]) { \
	table->leaf = 0; \
	nprev = NodeBuckets(table); \
    } else { \
	table->leaf = 1; \
	nprev = (NTable *)NhlMalloc(sizeof(VEntry *)); \
	if (!nprev) \
	    return; \
	((LTable)table)->buckets = (VEntry *)nprev; \
    } \
    *nprev = (NTable)NULL; \
    table->next = *prev; \
    *prev = table

    if (!db || !*quarks)
	return;
    table = *(prev = &db->table);
    /* if already at leaf, bump to the leaf table */
    if (!quarks[1] && table && !table->leaf)
	table = *(prev = &table->next);
    pprev = prev;
    if (!table || (quarks[1] && table->leaf)) {
	/* no top-level node table, create one and chain it in */
	NEWTABLE(NrmNULLQUARK,1);
	table->tight = 1; /* arbitrary */
	prev = nprev;
    } else {
	/* search along until we need a value */
	while (quarks[1]) {
	    q = *quarks;
	    table = *(prev = &NodeHash(table, q));
	    while (table && table->name != q)
		table = *(prev = &table->next);
	    if (!table)
		break; /* not found */
	    if (quarks[2]) {
		if (table->leaf)
		    break; /* not found */
	    } else {
		if (!table->leaf) {
		    /* bump to leaf table, if any */
		    table = *(prev = &table->next);
		    if (!table || table->name != q)
			break; /* not found */
		    if (!table->leaf) {
			/* bump to leaf table, if any */
			table = *(prev = &table->next);
			if (!table || table->name != q)
			    break; /* not found */
		    }
		}
	    }
	    if (*bindings == NrmBindTightly) {
		if (!table->tight)
		    break; /* not found */
	    } else {
		if (table->tight) {
		    /* bump to loose table, if any */
		    table = *(prev = &table->next);
		    if (!table || table->name != q ||
			!quarks[2] != table->leaf)
			break; /* not found */
		}
	    }
	    /* found that one, bump to next quark */
	    pprev = prev;
	    quarks++;
	    bindings++;
	}
	if (!quarks[1]) {
	    /* found all the way to a leaf */
	    q = *quarks;
	    entry = *(vprev = &LeafHash((LTable)table, q));
	    while (entry && entry->name != q)
		entry = *(vprev = &entry->next);
	    /* if want loose and have tight, bump to next entry */
	    if (entry && *bindings == NrmBindLoosely && entry->tight)
		entry = *(vprev = &entry->next);
	    if (entry && entry->name == q &&
		(*bindings == NrmBindTightly) == entry->tight) {
		/* match, need to override */
		if ((type == NrmQString) == entry->string &&
		    entry->size == value->size) {
		    /* update type if not String, can be different */
		    if (!entry->string)
			RepType(entry) = type;
		    /* identical size, just overwrite value */
		    memcpy(RawValue(entry),value->data.ptrval,value->size);
		    return;
		}
		/* splice out and free old entry */
		*vprev = entry->next;
		NhlFree((char *)entry);
		(*pprev)->entries--;
	    }
	    /* this is where to insert */
	    prev = (NTable *)vprev;
	}
    }
    /* keep the top table, because we may have to grow it */
    firstpprev = pprev;
    /* iterate until we get to the leaf */
    while (quarks[1]) {
	/* build a new table and chain it in */
	NEWTABLE(*quarks,2);
	if (*quarks++ == NrmQANY)
	    (*pprev)->hasany = 1;
	if (*bindings++ == NrmBindTightly) {
	    table->tight = 1;
	} else {
	    table->tight = 0;
	    (*pprev)->hasloose = 1;
	}
	(*pprev)->entries++;
	pprev = prev;
	prev = nprev;
    }
    /* now allocate the value entry */
    entry = (VEntry)NhlMalloc(((type == NrmQString) ?
			     sizeof(VEntryRec) : sizeof(DEntryRec)) +
			    value->size);
    if (!entry)
	return;
    entry->name = q = *quarks;
    if (*bindings == NrmBindTightly) {
	entry->tight = 1;
    } else {
	entry->tight = 0;
	(*pprev)->hasloose = 1;
    }
    /* chain it in, with a bit of type cast ugliness */
    entry->next = *((VEntry *)prev);
    *((VEntry *)prev) = entry;
    entry->size = value->size;
    if (type == NrmQString) {
	entry->string = 1;
    } else {
	entry->string = 0;
	RepType(entry) = type;
    }
    /* save a copy of the value */
    memcpy(RawValue(entry),value->data.ptrval,value->size);
    (*pprev)->entries++;
    /* this is a new leaf, need to remember it for search lists */
    if (q > maxResourceQuark) {
	unsigned oldsize = maxResourceQuark + 1;
	unsigned size = (q | 0x7f) + 1; /* reallocate in reasonable chunks */
	if (resourceQuarks)
	    resourceQuarks = (unsigned char *)NhlRealloc((char *)resourceQuarks,
						       size);
	else
	    resourceQuarks = (unsigned char *)NhlMalloc(size);
	if (resourceQuarks) {
	    memset((char*)&resourceQuarks[oldsize],0,size - oldsize);
	    maxResourceQuark = size - 1;
	} else {
	    maxResourceQuark = -1;
	}
    }
    if (q > 0 && resourceQuarks)
	resourceQuarks[q >> 3] |= 1 << (q & 0x7);
    GROW(firstpprev);

#undef NEWTABLE
}

void NrmQPutResource(pdb, bindings, quarks, type, value)
    NrmDatabase		*pdb;
    NrmBindingList      bindings;
    NrmQuarkList	quarks;
    NrmRepresentation	type;
    NrmValuePtr		value;
{
    if (!*pdb) *pdb = NewDatabase();
    PutEntry(*pdb, bindings, quarks, type, value);
}

#if NhlNeedProto
void NrmPutResource(
    NrmDatabase     *pdb,
    Const char    *specifier,
    Const char    *type,
    NrmValuePtr	    value)
#else
void NrmPutResource(pdb, specifier, type, value)
    NrmDatabase     *pdb;
    char	    *specifier;
    char	    *type;
    NrmValuePtr	    value;
#endif
{
    NrmBinding	    bindings[MANDBDEPTH+1];
    NrmQuark	    quarks[MANDBDEPTH+1];

    if (!*pdb) *pdb = NewDatabase();
    NrmStringToBindingQuarkList(specifier, bindings, quarks);
    PutEntry(*pdb, bindings, quarks, NrmStringToQuark(type), value);
}

#if NhlNeedProto
void NrmQPutStringResource(
    NrmDatabase     *pdb,
    NrmBindingList  bindings,
    NrmQuarkList    quarks,
    Const char    *str)
#else
void NrmQPutStringResource(pdb, bindings, quarks, str)
    NrmDatabase     *pdb;
    NrmBindingList  bindings;
    NrmQuarkList    quarks;
    char	    *str;
#endif
{
    NrmValue	value;

    if (!*pdb) *pdb = NewDatabase();
    value.data.ptrval = (NhlPointer) str;
    value.size = strlen(str)+1;
    PutEntry(*pdb, bindings, quarks, NrmQString, &value);
}

/*	Function Name: GetDatabase
 *	Description: Parses a string and stores it as a database.
 *	Arguments: db - the database.
 *                 str - a pointer to the string containing the database.
 *                 filename - source filename, if any.
 *                 doall - whether to do all lines or just one
 */

/*
 * This function is highly optimized to inline as much as possible. 
 * Be very careful with modifications, or simplifications, as they 
 * may adversely affect the performance.
 *
 * Chris Peterson, MIT N Consortium		5/17/90.
 */

#define LIST_SIZE 101
#define BUFFER_SIZE 100

static void GetIncludeFile();

static void GetDatabase(db, permstr, filename, doall)
    NrmDatabase db;
    Const char *permstr;
    Const char *filename;
    NhlBoolean doall;
{
    register char *ptr;
    register char *str = (char *)permstr;
    register NrmBits bits = 0;
    register char c;
    int len;
    register Signature sig;
    register char *ptr_max;
    register NrmQuarkList t_quarks;
    register NrmBindingList t_bindings;

    int alloc_chars = BUFSIZ;
    char buffer[BUFSIZ], *value_str;
    NrmQuark quarks[LIST_SIZE];
    NrmBinding bindings[LIST_SIZE];
    NrmValue value;
    NhlBoolean only_pcs;
    NhlBoolean dolines;

    if (!db)
	return;

    if ((value_str = (char *)NhlMalloc(sizeof(char) * alloc_chars))==NULL)
	return;

    /*
     * This suppress is fine untill we decide to add internationalization
     * to the package - db->mbstate isn't used db->methods->mbinit is a noop
     */
    /* SUPPRESS 113 */
    (*db->methods->mbinit)(db->mbstate);
    str--;
    dolines = True;
    while (!is_EOF(bits) && dolines) {
	dolines = doall;

	/*
	 * First: Remove extra whitespace. 
	 */

	do {
	    bits = next_char(c, str);
	} while is_space(bits);

	/*
	 * Ignore empty lines.
	 */

	if (is_EOL(bits))
	    continue;		/* start a new line. */

	/*
	 * Second: check the first character in a line to see if it is
	 * "!" signifying a comment, or "#" signifying a directive.
	 */

	if (c == '!') { /* Comment, spin to next newline */
	    /* SUPPRESS 570 */
	    while (is_simple(bits = next_char(c, str))) {}
	    if (is_EOL(bits))
		continue;
	    /* SUPPRESS 570 */
	    while (!is_EOL(bits = next_mbchar(c, len, str))) {}
	    str--;
	    continue;		/* start a new line. */
	}

	if (c == '#') { /* Directive */
	    /* remove extra whitespace */
	    only_pcs = True;
	    /* SUPPRESS 570 */
	    while (is_space(bits = next_char(c, str))) {};
	    /* only "include" directive is currently defined */
	    if (!strncmp(str, "include", 7)) {
		str += (7-1);
		/* remove extra whitespace */
		/* SUPPRESS 570 */
		while (is_space(bits = next_char(c, str))) {};
		/* must have a starting " */
		if (c == '"') {
		    char *fname = str+1;
		    len = 0;
		    do {
			if (only_pcs) {
			    bits = next_char(c, str);
			    if (is_nonpcs(bits))
				only_pcs = False;
			}
			if (!only_pcs)
			    bits = next_mbchar(c, len, str);
		    } while (c != '"' && !is_EOL(bits));
		    /* must have an ending " */
		    if (c == '"')
			GetIncludeFile(db, filename, fname, str - len - fname);
		}
	    }
	    /* spin to next newline */
	    if (only_pcs) {
		while (is_simple(bits))
		    bits = next_char(c, str);
		if (is_EOL(bits))
		    continue;
	    }
	    while (!is_EOL(bits))
		bits = next_mbchar(c, len, str);
	    str--;
	    continue;		/* start a new line. */
	}

	/*
	 * Third: loop through the LHS of the resource specification
	 * storing characters and converting this to a Quark.
	 *
	 * If the number of quarks is greater than LIST_SIZE - 1.  This
	 * function will trash your memory.
	 *
	 * If the length of any quark is larger than BUFSIZ this function
	 * will also trash memory.
	 */
	
	t_bindings = bindings;
	t_quarks = quarks;

	sig = 0;
	ptr = buffer;
	*t_bindings = NrmBindTightly;	
	for(;;) {
	    if (!is_binding(bits)) {
		while (!is_EOQ(bits)) {
		    *ptr++ = c;
		    sig = (sig << 1) + c; /* Compute the signature. */
		    bits = next_char(c, str);
		}

		*t_quarks++ = _NrmInternalStringToQuark(buffer, ptr - buffer,
							sig, False);

		if (is_separator(bits))  {
		    if (!is_space(bits))
			break;

		    /* Remove white space */
		    do {
			*ptr++ = c;
			sig = (sig << 1) + c; /* Compute the signature. */
		    } while (is_space(bits = next_char(c, str)));

		    /* 
		     * The spec doesn't permit it, but support spaces
		     * internal to resource name/class 
		     */

		    if (is_separator(bits))
			break;
		    t_quarks--;
		    continue;
		}

		if (c == '.')
		    *(++t_bindings) = NrmBindTightly;
		else
		    *(++t_bindings) = NrmBindLoosely;

		sig = 0;
		ptr = buffer;
	    }
	    else {
		/*
		 * Magic unspecified feature #254.
		 *
		 * If two separators appear with no Text between them then
		 * ignore them.
		 *
		 * If anyone of those separators is a '*' then the binding 
		 * will be loose, otherwise it will be tight.
		 */

		if (c == '*')
		    *t_bindings = NrmBindLoosely;
	    }

	    bits = next_char(c, str);
	}

	*t_quarks = NrmNULLQUARK;

	/*
	 * Make sure that there is a ':' in this line.
	 */

	if (c != ':') {
	    char oldc;

	    /*
	     * A parsing error has occured, toss everything on the line
	     * a new_line can still be escaped with a '\'.
	     */

	    while (is_normal(bits))
		bits = next_char(c, str);
	    if (is_EOL(bits))
		continue;
	    bits = next_mbchar(c, len, str);
	    do {
		oldc = c;
		bits = next_mbchar(c, len, str);
	    } while (c && (c != '\n' || oldc == '\\'));
	    str--;
	    continue;
	}

	/*
	 * I now have a quark and binding list for the entire left hand
	 * side.  "c" currently points to the ":" separating the left hand
	 * side for the right hand side.  It is time to begin processing
	 * the right hand side.
	 */

	/* 
	 * Fourth: Remove more whitespace
	 */

	for(;;) {
	    if (is_space(bits = next_char(c, str)))
		continue;
	    if (c != '\\')
		break;
	    bits = next_char(c, str);
	    if (c == '\n')
		continue;
	    str--;
	    bits = BSLASH;
	    c = '\\';
	    break;
	}

	/* 
	 * Fifth: Process the right hand side.
	 */

	ptr = value_str;
	ptr_max = ptr + alloc_chars - 4;
	only_pcs = True;
	len = 1;

	for(;;) {

	    /*
	     * Tight loop for the normal case:  Non backslash, non-end of value
	     * character that will fit into the allocated buffer.
	     */

	    if (only_pcs) {
		while (is_normal(bits) && ptr < ptr_max) {
		    *ptr++ = c;
		    bits = next_char(c, str);
		}
		if (is_EOL(bits))
		    break;
		if (is_nonpcs(bits)) {
		    only_pcs = False;
		    bits = next_mbchar(c, len, str);
		}
	    }
	    while (!is_special(bits) && ptr + len <= ptr_max) {
		len = -len;
		while (len)
		    *ptr++ = str[len++];
		bits = next_mbchar(c, len, str);
	    }

	    if (is_EOL(bits)) {
		str--;
		break;
	    }

	    if (c == '\\') {
		/*
		 * We need to do some magic after a backslash.
		 */

		if (only_pcs) {
		    bits = next_char(c, str);
		    if (is_nonpcs(bits))
			only_pcs = False;
		}
		if (!only_pcs)
		    bits = next_mbchar(c, len, str);

		if (is_EOL(bits)) {
		    if (is_EOF(bits))
			continue;
		} else if (c == 'n') {
		    /*
		     * "\n" means insert a newline.
		     */
		    *ptr++ = '\n';
		} else if (c == '\\') {
		    /*
		     * "\\" completes to just one backslash.
		     */
		    *ptr++ = '\\';
		} else {
		    /*
		     * pick up to three octal digits after the '\'.
		     */
		    char temp[3];
		    int count = 0;
		    while (is_odigit(bits) && count < 3) {
			temp[count++] = c;
			if (only_pcs) {
			    bits = next_char(c, str);
			    if (is_nonpcs(bits))
				only_pcs = False;
			}
			if (!only_pcs)
			    bits = next_mbchar(c, len, str);
		    }

		    /*
		     * If we found three digits then insert that octal code
		     * into the value string as a character.
		     */

		    if (count == 3) {
			*ptr++ = (unsigned char) ((temp[0] - '0') * 0100 +
						  (temp[1] - '0') * 010 +
						  (temp[2] - '0'));
		    }
		    else {
			int tcount;

			/* 
			 * Otherwise just insert those characters into the 
			 * string, since no special processing is needed on
			 * numerics we can skip the special processing.
			 */

			for (tcount = 0; tcount < count; tcount++) {
			    *ptr++ = temp[tcount]; /* print them in
						      the correct order */
			}
		    }
		    continue;
		}
		if (only_pcs) {
		    bits = next_char(c, str);
		    if (is_nonpcs(bits))
			only_pcs = False;
		}
		if (!only_pcs)
		    bits = next_mbchar(c, len, str);
	    }

	    /* 
	     * It is important to make sure that there is room for at least
	     * four more characters in the buffer, since I can add that
	     * many characters into the buffer after a backslash has occured.
	     */

	    if (ptr + len > ptr_max) {
		char * temp_str;

		alloc_chars += BUFSIZ/10;		
		temp_str = (char *)NhlRealloc(value_str,
						sizeof(char) * alloc_chars);

		if (!temp_str) {
		    NhlFree(value_str);
		    (*db->methods->mbfinish)(db->mbstate);
		    return;
		}

		ptr = temp_str + (ptr - value_str); /* reset pointer. */
		value_str = temp_str;
		ptr_max = value_str + alloc_chars - 4;
	    }
	}

	/*
	 * Lastly: Terminate the value string, and store this entry 
	 * 	   into the database.
	 */

	*ptr++ = '\0';

	/* Store it in database */
	value.size = ptr - value_str;
	value.data.ptrval = (NhlPointer) value_str;
	
	PutEntry(db, bindings, quarks, NrmQString, &value);
    }

    NhlFree(value_str);
    /*
     * This suppress is fine untill we decide to add internationalization
     * to the package - db->mbstate isn't used db->methods->mbinit is a noop
     */
    /* SUPPRESS 113 */
    (*db->methods->mbfinish)(db->mbstate);
}

void
NrmPutStringRes
#if	NhlNeedProto
(
	NrmDatabase	*pdb,
	Const char	*specifier,
	Const char	*str
)
#else
(pdb, specifier, str)
	NrmDatabase	*pdb;
	Const char	*specifier;
	Const char	*str;
#endif
{
	NrmValue	value;
	NrmBinding	bindings[MANDBDEPTH+1];
	NrmQuark	quarks[MANDBDEPTH+1];

	if (!*pdb) *pdb = NewDatabase();
	NrmStringToBindingQuarkList(specifier, bindings, quarks);
	value.data.ptrval = (NhlPointer) str;
	value.size = strlen(str)+1;
	PutEntry(*pdb, bindings, quarks, NrmQString, &value);
}


#if NhlNeedProto
void NrmPutLineResource(
    NrmDatabase *pdb,
    Const char*line)
#else
void NrmPutLineResource(pdb, line)
    NrmDatabase *pdb;
    Const char	*line;
#endif
{
    if (!*pdb) *pdb = NewDatabase();
    GetDatabase(*pdb, line, (char *)NULL, False);
}

#if NhlNeedProto
NrmDatabase NrmGetStringDatabase(
    Const char    *data)
#else
NrmDatabase NrmGetStringDatabase(data)
    Const char	    *data;
#endif
{
    NrmDatabase     db;

    db = NewDatabase();
    GetDatabase(db, data, (char *)NULL, True);
    return db;
}

/*	Function Name: ReadInFile
 *	Description: Reads the file into a buffer.
 *	Arguments: filename - the name of the file.
 *	Returns: An allocated string containing the contents of the file.
 */

static char *
ReadInFile(filename)
Const char * filename;
{
    register int fd, size;
    char * filebuf;

    if ( (fd = OpenFile(filename)) == -1 )
	return (char *)NULL;

    GetSizeOfFile(filename, size);
	
    if((filebuf = (char *)NhlMalloc(size + 1))==NULL){ /* leave room for '\0' */
	close(fd);
	return (char *)NULL;
    }

    if (ReadFile(fd, filebuf, size) != size) { /* If we didn't read the
						  correct number of bytes. */
	CloseFile(fd);
	NhlFree(filebuf);
	return (char *)NULL;
    }
    CloseFile(fd);

    filebuf[size] = '\0';	/* NULL terminate it. */
    return filebuf;
}

static void
GetIncludeFile(db, base, fname, fnamelen)
    NrmDatabase db;
    char *base;
    char *fname;
    int fnamelen;
{
    int len;
    char *str;
    char realfname[BUFSIZ];

    if (fnamelen <= 0 || fnamelen >= BUFSIZ)
	return;
    if (fname[0] != '/' && base && ((str = strrchr(base, '/'))!=NULL)) {
	len = str - base + 1;
	if (len + fnamelen >= BUFSIZ)
	    return;
	strncpy(realfname, base, len);
	strncpy(realfname + len, fname, fnamelen);
	realfname[len + fnamelen] = '\0';
    } else {
	strncpy(realfname, fname, fnamelen);
	realfname[fnamelen] = '\0';
    }
    if ((str = ReadInFile(realfname)) == NULL)
	return;
    GetDatabase(db, str, realfname, True);
    NhlFree(str);
}

NrmDatabase NrmGetFileDB
#if NhlNeedProto
(
	Const char	*filename
)
#else
(filename)
	Const char	*filename;
#endif
{
	NrmDatabase db;
	char *str;

	if(filename == (char*)NULL)
		return (NrmDatabase)NULL;

	if ((str = ReadInFile(filename)) == NULL)
		return (NrmDatabase)NULL;

	db = NewDatabase();
	GetDatabase(db, str, filename, True);
	NhlFree(str);
	return db;
}

int
NrmCombineFileDB
#if	NhlNeedProto
(
    Const char    *filename,
    NrmDatabase     *target,
    NhlBoolean             override
)
#else
(filename, target, override)
    char        *filename;
    NrmDatabase *target;
    NhlBoolean         override;
#endif
{
    NrmDatabase db;
    char *str;

	if(filename == (char*)NULL)
		return 0;

	if ((str = ReadInFile(filename)) == NULL)
		return 0;

	if (override) {
		db = *target;
		if (!db)
		*target = db = NewDatabase();
	} else
		db = NewDatabase();
	GetDatabase(db, str, filename, True);
	NhlFree(str);
	if (!override)
		NrmCombineDatabase(db, target, False);
	return 1;
}

/* call the user proc for every value in the table, arbitrary order.
 * stop if user proc returns True.  level is current depth in database.
 */
/*ARGSUSED*/
static NhlBoolean EnumLTable(table, names, classes, level, closure)
    LTable		table;
    NrmNameList		names;
    NrmClassList 	classes;
    register int	level;
    register EClosure	closure;
{
    register VEntry *bucket;
    register int i;
    register VEntry entry;
    NrmValue value;
    NrmRepresentation type;
    NhlBoolean tightOk;

    closure->bindings[level] = (table->table.tight ?
				NrmBindTightly : NrmBindLoosely);
    closure->quarks[level] = table->table.name;
    level++;
    tightOk = !*names;
    closure->quarks[level + 1] = NrmNULLQUARK;
    for (i = table->table.mask, bucket = table->buckets;
	 i >= 0;
	 i--, bucket++) {
	for (entry = *bucket; entry; entry = entry->next) {
	    if (entry->tight && !tightOk)
		continue;
	    closure->bindings[level] = (entry->tight ?
					NrmBindTightly : NrmBindLoosely);
	    closure->quarks[level] = entry->name;
	    value.size = entry->size;
	    if (entry->string) {
		type = NrmQString;
		value.data.ptrval = StringValue(entry);
	    } else {
		type = RepType(entry);
		value.data.ptrval = DataValue(entry);
	    }
	    if ((*closure->proc)(&closure->db, closure->bindings+1,
				 closure->quarks+1, &type, &value,
				 closure->closure))
		return True;
	}
    }
    return False;
}

static NhlBoolean EnumAllNTable(table, level, closure)
    NTable		table;
    register int	level;
    register EClosure	closure;
{
    register NTable *bucket;
    register int i;
    register NTable entry;
    NrmQuark empty = NrmNULLQUARK;

    if (level >= MANDBDEPTH)
	return False;
    for (i = table->mask, bucket = NodeBuckets(table);
	 i >= 0;
	 i--, bucket++) {
	for (entry = *bucket; entry; entry = entry->next) {
	    if (entry->leaf) {
		if (EnumLTable((LTable)entry, &empty, &empty, level, closure))
		    return True;
	    } else {
		closure->bindings[level] = (entry->tight ?
					    NrmBindTightly : NrmBindLoosely);
		closure->quarks[level] = entry->name;
		if (EnumAllNTable(entry, level+1, closure))
		    return True;
	    }
	}
    }
    return False;
}

/* recurse on every table in the table, arbitrary order.
 * stop if user proc returns True.  level is current depth in database.
 */
static NhlBoolean EnumNTable(table, names, classes, level, closure)
    NTable		table;
    NrmNameList		names;
    NrmClassList 	classes;
    register int	level;
    register EClosure	closure;
{
    register NTable	entry;
    register NrmQuark	q;
    register unsigned int leaf;
    NhlBoolean (*get)();
    NhlBoolean bilevel;

/* find entries named ename, leafness leaf, tight or loose, and call get */
#define ITIGHTLOOSE(ename) \
    NFIND(ename); \
    if (entry) { \
	if (leaf == entry->leaf) { \
	    if (!leaf && !entry->tight && entry->next && \
		entry->next->name == q && entry->next->tight && \
		(bilevel || entry->next->hasloose) && \
		EnumLTable((LTable)entry->next, names+1, classes+1, \
			   level, closure)) \
		return True; \
	    if ((*get)(entry, names+1, classes+1, level, closure)) \
		return True; \
	    if (entry->tight && ((entry=entry->next)!=NULL) && \
		entry->name == q && leaf == entry->leaf && \
		(*get)(entry, names+1, classes+1, level, closure)) \
		return True; \
	} else if (entry->leaf) { \
	    if ((bilevel || entry->hasloose) && \
		EnumLTable((LTable)entry, names+1, classes+1, level, closure))\
		return True; \
	    if (entry->tight && ((entry=entry->next)!=NULL) && \
		entry->name == q && (bilevel || entry->hasloose) && \
		EnumLTable((LTable)entry, names+1, classes+1, level, closure))\
		return True; \
	} \
    }

/* find entries named ename, leafness leaf, loose only, and call get */
#define ILOOSE(ename) \
    NFIND(ename); \
    if (entry && entry->tight&&((entry=entry->next)!=NULL)&&entry->name != q) \
	entry = (NTable)NULL; \
    if (entry) { \
	if (leaf == entry->leaf) { \
	    if ((*get)(entry, names+1, classes+1, level, closure)) \
		return True; \
	} else if (entry->leaf && (bilevel || entry->hasloose)) { \
	    if (EnumLTable((LTable)entry, names+1, classes+1, level, closure))\
		return True; \
	} \
    }

    if (level >= MANDBDEPTH)
	return False;
    closure->bindings[level] = (table->tight ?
				NrmBindTightly : NrmBindLoosely);
    closure->quarks[level] = table->name;
    level++;
    if (!*names) {
	if (EnumAllNTable(table, level, closure))
	    return True;
    } else {
	if (names[1] || closure->mode == NrmEnumAllLevels) {
	    get = EnumNTable; /* recurse */
	    leaf = 0;
	    bilevel = !names[1];
	} else {
	    get = EnumLTable; /* bottom of recursion */
	    leaf = 1;
	    bilevel = False;
	}
	if (table->hasloose && closure->mode == NrmEnumAllLevels) {
	    NTable *bucket;
	    int i;
	    NrmQuark empty = NrmNULLQUARK;

	    for (i = table->mask, bucket = NodeBuckets(table);
		 i >= 0;
		 i--, bucket++) {
		q = NrmNULLQUARK;
		for (entry = *bucket; entry; entry = entry->next) {
		    if (!entry->tight && entry->name != q &&
			entry->name != *names && entry->name != *classes) {
			q = entry->name;
			if (entry->leaf) {
			    if (EnumLTable((LTable)entry, &empty, &empty,
					   level, closure))
				return True;
			} else {
			    if (EnumNTable(entry, &empty, &empty,
					   level, closure))
				return True;
			}
		    }
		}
	    }
	}

	ITIGHTLOOSE(*names);   /* do name, tight and loose */
	ITIGHTLOOSE(*classes); /* do class, tight and loose */
	if (table->hasany) {
	    ITIGHTLOOSE(NrmQANY); /* do ANY, tight and loose */
	}
	if (table->hasloose) {
	    while (1) {
		names++;
		classes++;
		if (!*names)
		    break;
		if (!names[1] && closure->mode != NrmEnumAllLevels) {
		    get = EnumLTable; /* bottom of recursion */
		    leaf = 1;
		}
		ILOOSE(*names);   /* loose names */
		ILOOSE(*classes); /* loose classes */
		if (table->hasany) {
		    ILOOSE(NrmQANY); /* loose ANY */
		}
	    }
	    names--;
	    classes--;
	}
    }
    /* now look for matching leaf nodes */
    entry = table->next;
    if (!entry)
	return False;
    if (entry->leaf) {
	if (entry->tight && !table->tight)
	    entry = entry->next;
    } else {
	entry = entry->next;
	if (!entry || !entry->tight)
	    return False;
    }
    if (!entry || entry->name != table->name)
	return False;
    /* found one */
    level--;
    if ((!*names || entry->hasloose) &&
	EnumLTable((LTable)entry, names, classes, level, closure))
	return True;
    if (entry->tight && entry == table->next && ((entry=entry->next)!=NULL) &&
	entry->name == table->name && (!*names || entry->hasloose))
	return EnumLTable((LTable)entry, names, classes, level, closure);
    return False;

#undef ITIGHTLOOSE
#undef ILOOSE
}

/* call the proc for every value in the database, arbitrary order.
 * stop if the proc returns True.
 */
NhlBoolean NrmEnumerateDatabase(db, names, classes, mode, proc, closure)
    NrmDatabase		db;
    NrmNameList		names;
    NrmClassList	classes;
    int			mode;
    NrmDBEnumProc		proc;
    NhlPointer		closure;
{
    NrmBinding  bindings[MANDBDEPTH+2];
    NrmQuark	quarks[MANDBDEPTH+2];
    register NTable table;
    EClosureRec	eclosure;

    if (!db)
	return False;
    eclosure.db = db;
    eclosure.proc = proc;
    eclosure.closure = closure;
    eclosure.bindings = bindings;
    eclosure.quarks = quarks;
    eclosure.mode = mode;
    table = db->table;
    if (table && !table->leaf && !*names && mode == NrmEnumOneLevel)
	table = table->next;
    if (table) {
	if (!table->leaf)
	    return EnumNTable(table, names, classes, 0, &eclosure);
	else
	    return EnumLTable((LTable)table, names, classes, 0, &eclosure);
    }
    return False;
}

static void PrintBindingQuarkList(bindings, quarks, stream)
    NrmBindingList      bindings;
    NrmQuarkList	quarks;
    FILE		*stream;
{
    NhlBoolean	firstNameSeen;

    for (firstNameSeen = False; *quarks; bindings++, quarks++) {
	if (*bindings == NrmBindLoosely) {
	    (void) fprintf(stream, "*");
	} else if (firstNameSeen) {
	    (void) fprintf(stream, ".");
	}
	firstNameSeen = True;
	(void) fputs(NrmQuarkToString(*quarks), stream);
    }
}

/* output out the entry in correct file syntax */
/*ARGSUSED*/
static NhlBoolean DumpEntry(db, bindings, quarks, type, value, data)
    NrmDatabase		*db;
    NrmBindingList      bindings;
    NrmQuarkList	quarks;
    NrmRepresentation   *type;
    NrmValuePtr		value;
    NhlPointer		data;
{
    FILE			*stream = (FILE *)data;
    register unsigned int	i;
    register char		*s;
    register char		c;

    if (*type != NrmQString)
	(void) putc('!', stream);
    PrintBindingQuarkList(bindings, quarks, stream);
    s = value->data.ptrval;
    i = value->size;
    if (*type == NrmQString) {
	(void) fputs(":\t", stream);
	if (i)
	    i--;
    }
    else
	fprintf(stream, "=%s:\t", NrmRepresentationToString(*type));
    if (i && (*s == ' ' || *s == '\t'))
	(void) putc('\\', stream); /* preserve leading whitespace */
    while (i--) {
	c = *s++;
	if (c == '\n') {
	    if (i)
		(void) fputs("\\n\\\n", stream);
	    else
		(void) fputs("\\n", stream);
	} else if (c == '\\')
	    (void) fputs("\\\\", stream);
	else if ((c < ' ' && c != '\t') ||
		 ((unsigned char)c >= 0x7f && (unsigned char)c < 0xa0))
	    (void) fprintf(stream, "\\%03o", (unsigned char)c);
	else
	    (void) putc(c, stream);
    }
    (void) putc('\n', stream);
    return False;
}

#ifdef DEBUG

void PrintTable(table, file)
    NTable table;
    FILE *file;
{
    NrmBinding  bindings[MANDBDEPTH+1];
    NrmQuark	quarks[MANDBDEPTH+1];
    EClosureRec closure;
    NrmQuark	empty = NrmNULLQUARK;

    closure.db = (NrmDatabase)NULL;
    closure.proc = DumpEntry;
    closure.closure = (NhlPointer)file;
    closure.bindings = bindings;
    closure.quarks = quarks;
    closure.mode = NrmEnumAllLevels;
    if (table->leaf)
	EnumLTable((LTable)table, &empty, &empty, 0, &closure);
    else
	EnumNTable(table, &empty, &empty, 0, &closure);
}

#endif /* DEBUG */

#if NhlNeedProto
void NrmPutFileDatabase(
    NrmDatabase db,
    Const char *fileName)
#else
void NrmPutFileDatabase(db, fileName)
    NrmDatabase db;
    char 	*fileName;
#endif
{
    FILE	*file;
    NrmQuark empty = NrmNULLQUARK;

    if (!db) return;
    if ((file = fopen(fileName, "w")) == NULL) return;
    (void)NrmEnumerateDatabase(db, &empty, &empty, NrmEnumAllLevels,
			       DumpEntry, (NhlPointer) file);
    fclose(file);
}

/* macros used in get/search functions */

/* find entries named ename, leafness leaf, tight or loose, and call get */
#define GTIGHTLOOSE(ename,looseleaf) \
    NFIND(ename); \
    if (entry) { \
	if (leaf == entry->leaf) { \
	    if (!leaf && !entry->tight && entry->next && \
		entry->next->name == q && entry->next->tight && \
		entry->next->hasloose && \
		looseleaf((LTable)entry->next, names+1, classes+1, closure)) \
		return True; \
	    if ((*get)((LTable)entry, names+1, classes+1, closure)) \
		return True; \
	    if (entry->tight && ((entry=entry->next)!=NULL) && \
		(entry->name == q) && (leaf == entry->leaf) && \
		(*get)((LTable)entry, names+1, classes+1, closure)) \
		return True; \
	} else if (entry->leaf) { \
	    if (entry->hasloose && \
		looseleaf((LTable)entry, names+1, classes+1, closure)) \
		return True; \
	    if (entry->tight && ((entry=entry->next)!=NULL) && \
		(entry->name == q) && entry->hasloose && \
		looseleaf((LTable)entry, names+1, classes+1, closure)) \
		return True; \
	} \
    }

/* find entries named ename, leafness leaf, loose only, and call get */
#define GLOOSE(ename,looseleaf) \
    NFIND(ename); \
    if (entry && entry->tight&&((entry=entry->next)!=NULL)&&(entry->name!=q)) \
	entry = (NTable)NULL; \
    if (entry) { \
	if (leaf == entry->leaf) { \
	    if ((*get)((LTable)entry, names+1, classes+1, closure)) \
		return True; \
	} else if (entry->leaf && entry->hasloose) { \
	    if (looseleaf((LTable)entry, names+1, classes+1, closure)) \
		return True; \
	} \
    }

/* add tight/loose entry to the search list, return True if list is full */
/*ARGSUSED*/
static NhlBoolean AppendLEntry(table, names, classes, closure)
    LTable		table;
    NrmNameList		names;
    NrmClassList 	classes;
    register SClosure	closure;
{
    /* check for duplicate */
    if (closure->idx >= 0 && closure->list[closure->idx] == table)
	return False;
    if (closure->idx == closure->limit)
	return True;
    /* append it */
    closure->idx++;
    closure->list[closure->idx] = table;
    return False;
}

/* add loose entry to the search list, return True if list is full */
/*ARGSUSED*/
static NhlBoolean AppendLooseLEntry(table, names, classes, closure)
    LTable		table;
    NrmNameList		names;
    NrmClassList 	classes;
    register SClosure	closure;
{
    /* check for duplicate */
    if (closure->idx >= 0 && closure->list[closure->idx] == table)
	return False;
    if (closure->idx >= closure->limit - 1)
	return True;
    /* append it */
    closure->idx++;
    closure->list[closure->idx] = LOOSESEARCH;
    closure->idx++;
    closure->list[closure->idx] = table;
    return False;
}

/* search for a leaf table */
static NhlBoolean SearchNEntry(table, names, classes, closure)
    NTable		table;
    NrmNameList		names;
    NrmClassList 	classes;
    SClosure		closure;
{
    register NTable	entry;
    register NrmQuark	q;
    register unsigned int leaf;
    NhlBoolean		(*get)();

    if (names[1]) {
	get = SearchNEntry; /* recurse */
	leaf = 0;
    } else {
	get = AppendLEntry; /* bottom of recursion */
	leaf = 1;
    }
    GTIGHTLOOSE(*names, AppendLooseLEntry);   /* do name, tight and loose */
    GTIGHTLOOSE(*classes, AppendLooseLEntry); /* do class, tight and loose */
    if (table->hasany) {
	GTIGHTLOOSE(NrmQANY, AppendLooseLEntry); /* do ANY, tight and loose */
    }
    if (table->hasloose) {
	while (1) {
	    names++;
	    classes++;
	    if (!*names)
		break;
	    if (!names[1]) {
		get = AppendLEntry; /* bottom of recursion */
		leaf = 1;
	    }
	    GLOOSE(*names, AppendLooseLEntry);   /* loose names */
	    GLOOSE(*classes, AppendLooseLEntry); /* loose classes */
	    if (table->hasany) {
		GLOOSE(NrmQANY, AppendLooseLEntry); /* loose ANY */
	    }
	}
    }
    /* now look for matching leaf nodes */
    entry = table->next;
    if (!entry)
	return False;
    if (entry->leaf) {
	if (entry->tight && !table->tight)
	    entry = entry->next;
    } else {
	entry = entry->next;
	if (!entry || !entry->tight)
	    return False;
    }
    if (!entry || entry->name != table->name)
	return False;
    /* found one */
    if (entry->hasloose &&
	AppendLooseLEntry((LTable)entry, names, classes, closure))
	return True;
    if (entry->tight && entry == table->next && ((entry=entry->next)!=NULL) &&
	entry->name == table->name && entry->hasloose)
	return AppendLooseLEntry((LTable)entry, names, classes, closure);
    return False;
}

NhlBoolean NrmQGetSearchList(db, names, classes, searchList, listLength)
    NrmDatabase     db;
    NrmNameList	    names;
    NrmClassList    classes;
    NrmSearchList   searchList;	/* RETURN */
    int		    listLength;
{
    register NTable	table;
    SClosureRec		closure;

    if (listLength <= 0)
	return False;
    closure.list = (LTable *)searchList;
    closure.idx = -1;
    closure.limit = listLength - 2;
    if (db) {
	table = db->table;
	if (*names) {
	    if (table && !table->leaf) {
		if (SearchNEntry(table, names, classes, &closure))
		    return False;
	    } else if (table && table->hasloose &&
		       AppendLooseLEntry((LTable)table, names, classes,
					 &closure))
		return False;
	} else {
	    if (table && !table->leaf)
		table = table->next;
	    if (table && AppendLEntry((LTable)table, names, classes, &closure))
		return False;
	}
    }
    closure.list[closure.idx + 1] = (LTable)NULL;
    return True;
}

NhlBoolean
NrmGetQResFromList
#if	NhlNeedProto
(
	NrmSearchList		searchList,
	register NrmName	name,
	register NrmClass	class,
	NrmRepresentation	*pType,		/* RETURN */
	NrmValue		*pValue		/* RETURN */
)
#else
(searchList, name, class, pType, pValue)
	NrmSearchList		searchList;
	register NrmName	name;
	register NrmClass	class;
	NrmRepresentation	*pType;		/* RETURN */
	NrmValue		*pValue;	/* RETURN */
#endif
{
    register LTable *list;
    register LTable table;
    register VEntry entry=NULL;
    int flags;

/* find tight or loose entry */
#define VTIGHTLOOSE(q) \
    entry = LeafHash(table, q); \
    while (entry && entry->name != q) \
	entry = entry->next; \
    if (entry) \
	break

/* find loose entry */
#define VLOOSE(q) \
    entry = LeafHash(table, q); \
    while (entry && (entry->name != q)){ \
	entry = entry->next; \
    } \
    if (entry) { \
	if (!entry->tight) \
	    break; \
	entry = entry->next; \
	if (entry && (entry->name == q)) \
	    break; \
    }

    list = (LTable *)searchList;
    /* figure out which combination of name and class we need to search for */
    flags = 0;
    if (IsResourceQuark(name))
	flags = 2;
    if (IsResourceQuark(class))
	flags |= 1;
    if (!flags) {
	/* neither name nor class has ever been used to name a resource */
	table = (LTable)NULL;
    } else if (flags == 3) {
	/* both name and class */
	/* SUPPRESS 624 */
	while ((table = *list++) != NULL) {
	    if (table != LOOSESEARCH) {
		VTIGHTLOOSE(name);  /* do name, tight and loose */
		VTIGHTLOOSE(class); /* do class, tight and loose */
	    } else {
		table = *list++;
		VLOOSE(name);  /* do name, loose only */
		VLOOSE(class); /* do class, loose only */
	    }
	}
    } else {
	/* just one of name or class */
	if (flags == 1)
	    name = class;
	/* SUPPRESS 624 */
	while ((table = *list++) != NULL) {
	    if (table != LOOSESEARCH) {
		VTIGHTLOOSE(name); /* tight and loose */
	    } else {
		table = *list++;
		VLOOSE(name); /* loose only */
	    }
	}
    }
    if (table) {
	/* found a match */
	if (entry->string) {
	    *pType = NrmQString;
	    pValue->data.ptrval = StringValue(entry);
	} else {
	    *pType = RepType(entry);
	    pValue->data.ptrval = DataValue(entry);
	}
	pValue->size = entry->size;
	return True;
    }
    *pType = NrmNULLQUARK;
    pValue->data.ptrval = (NhlPointer)NULL;
    pValue->size = 0;
    return False;

#undef VTIGHTLOOSE
#undef VLOOSE
}

/* look for a tight/loose value */
static NhlBoolean GetVEntry(table, names, classes, closure)
    LTable		table;
    NrmNameList		names;
    NrmClassList 	classes;
    VClosure		closure;
{
    register VEntry entry;
    register NrmQuark q;

    /* try name first */
    q = *names;
    entry = LeafHash(table, q);
    while (entry && entry->name != q)
	entry = entry->next;
    if (!entry) {
	/* not found, try class */
	q = *classes;
	entry = LeafHash(table, q);
	while (entry && entry->name != q)
	    entry = entry->next;
	if (!entry)
	    return False;
    }
    if (entry->string) {
	*closure->type = NrmQString;
	closure->value->data.ptrval = StringValue(entry);
    } else {
	*closure->type = RepType(entry);
	closure->value->data.ptrval = DataValue(entry);
    }
    closure->value->size = entry->size;
    return True;
}

/* look for a loose value */
static NhlBoolean GetLooseVEntry(table, names, classes, closure)
    LTable		table;
    NrmNameList		names;
    NrmClassList 	classes;
    VClosure		closure;
{
    register VEntry	entry;
    register NrmQuark	q;

#define VLOOSE(ename) \
    q = ename; \
    entry = LeafHash(table, q); \
    while (entry && entry->name != q) \
	entry = entry->next; \
    if (entry && entry->tight && ((entry=entry->next)!=NULL)&& entry->name!=q) \
	entry = (VEntry)NULL;

    /* bump to last component */
    while (names[1]) {
	names++;
	classes++;
    }
    VLOOSE(*names);  /* do name, loose only */
    if (!entry) {
	VLOOSE(*classes); /* do class, loose only */
	if (!entry)
	    return False;
    }
    if (entry->string) {
	*closure->type = NrmQString;
	closure->value->data.ptrval = StringValue(entry);
    } else {
	*closure->type = RepType(entry);
	closure->value->data.ptrval = DataValue(entry);
    }
    closure->value->size = entry->size;
    return True;

#undef VLOOSE
}

/* recursive search for a value */
static NhlBoolean GetNEntry(table, names, classes, closure)
    NTable		table;
    NrmNameList		names;
    NrmClassList 	classes;
    VClosure		closure;
{
    register NTable	entry;
    register NrmQuark	q;
    register unsigned int leaf;
    NhlBoolean		(*get)();
    NTable		otable;

    if (names[2]) {
	get = GetNEntry; /* recurse */
	leaf = 0;
    } else {
	get = GetVEntry; /* bottom of recursion */
	leaf = 1;
    }
    GTIGHTLOOSE(*names, GetLooseVEntry);   /* do name, tight and loose */
    GTIGHTLOOSE(*classes, GetLooseVEntry); /* do class, tight and loose */
    if (table->hasany) {
	GTIGHTLOOSE(NrmQANY, GetLooseVEntry); /* do ANY, tight and loose */
    }
    if (table->hasloose) {
	while (1) {
	    names++;
	    classes++;
	    if (!names[1])
		break;
	    if (!names[2]) {
		get = GetVEntry; /* bottom of recursion */
		leaf = 1;
	    }
	    GLOOSE(*names, GetLooseVEntry);   /* do name, loose only */
	    GLOOSE(*classes, GetLooseVEntry); /* do class, loose only */
	    if (table->hasany) {
		GLOOSE(NrmQANY, GetLooseVEntry); /* do ANY, loose only */
	    }
	}
    }
    /* look for matching leaf tables */
    otable = table;
    table = table->next;
    if (!table)
	return False;
    if (table->leaf) {
	if (table->tight && !otable->tight)
	    table = table->next;
    } else {
	table = table->next;
	if (!table || !table->tight)
	    return False;
    }
    if (!table || table->name != otable->name)
	return False;
    /* found one */
    if (table->hasloose &&
	GetLooseVEntry((LTable)table, names, classes, closure))
	return True;
    if (table->tight && table == otable->next) {
	table = table->next;
	if (table && table->name == otable->name && table->hasloose)
	    return GetLooseVEntry((LTable)table, names, classes, closure);
    }
    return False;
}

NhlBoolean NrmQGetResource(db, names, classes, pType, pValue)
    NrmDatabase         db;
    NrmNameList		names;
    NrmClassList 	classes;
    NrmRepresentation	*pType;  /* RETURN */
    NrmValuePtr		pValue;  /* RETURN */
{
    register NTable table;
    VClosureRec closure;

    if (db && *names) {
	closure.type = pType;
	closure.value = pValue;
	table = db->table;
	if (names[1]) {
	    if (table && !table->leaf) {
		if (GetNEntry(table, names, classes, &closure))
		    return True;
	    } else if (table && table->hasloose &&
		       GetLooseVEntry((LTable)table, names, classes, &closure))
		return True;
	} else {
	    if (table && !table->leaf)
		table = table->next;
	    if (table && GetVEntry((LTable)table, names, classes, &closure))
		return True;
	}
    }
    *pType = NrmNULLQUARK;
    pValue->data.ptrval = (NhlPointer)NULL;
    pValue->size = 0;
    return False;
}

#if NhlNeedProto
NhlBoolean NrmGetResource(db, name_str, class_str, pType_str, pValue)
    NrmDatabase         db;
    Const char	*name_str;
    Const char	*class_str;
    NrmString		*pType_str;  /* RETURN */
    NrmValuePtr		pValue;      /* RETURN */
#else
NhlBoolean NrmGetResource(db, name_str, class_str, pType_str, pValue)
    NrmDatabase         db;
    NrmString		name_str;
    NrmString		class_str;
    NrmString		*pType_str;  /* RETURN */
    NrmValuePtr		pValue;      /* RETURN */
#endif
{
    NrmName		names[MANDBDEPTH+1];
    NrmClass		classes[MANDBDEPTH+1];
    NrmRepresentation   fromType;
    NhlBoolean		result;

    NrmStringToNameList(name_str, names);
    NrmStringToClassList(class_str, classes);
    result = NrmQGetResource(db, names, classes, &fromType, pValue);
    (*pType_str) = NrmQuarkToString(fromType);
    return result;
}

/* destroy all values, plus table itself */
static void DestroyLTable(table)
    LTable table;
{
    register int i;
    register VEntry *buckets;
    register VEntry entry, next;

    buckets = table->buckets;
    for (i = table->table.mask; i >= 0; i--, buckets++) {
	/* SUPPRESS 624 */
	for (next = *buckets; ((entry=next)!=NULL); ) {
	    next = entry->next;
	    NhlFree((char *)entry);
	}
    }
    NhlFree((char *)table->buckets);
    NhlFree((char *)table);
}

/* destroy all contained tables, plus table itself */
static void DestroyNTable(table)
    NTable table;
{
    register int i;
    register NTable *buckets;
    register NTable entry, next;

    buckets = NodeBuckets(table);
    for (i = table->mask; i >= 0; i--, buckets++) {
	/* SUPPRESS 624 */
	for (next = *buckets; ((entry=next) != NULL); ) {
	    next = entry->next;
	    if (entry->leaf)
		DestroyLTable((LTable)entry);
	    else
		DestroyNTable(entry);
	}
    }
    NhlFree((char *)table);
}

char *NrmLocaleOfDatabase(db)
    NrmDatabase db;
{
    return (*db->methods->lcname)(db->mbstate);
}

void
NrmDestroyDB
#if	NhlNeedProto
(
	NrmDatabase	db
)
#else
(db)
	NrmDatabase	db;
#endif
{
    register NTable table, next;

    if (db) {
	/* SUPPRESS 624 */
	for (next = db->table; ((table=next)!=NULL); ) {
	    next = table->next;
	    if (table->leaf)
		DestroyLTable((LTable)table);
	    else
		DestroyNTable(table);
	}
	/* SUPPRESS 113 */
	(*db->methods->destroy)(db->mbstate);
	NhlFree((char *)db);
    }
}


/*
 * Function:	NrmQinQList
 *
 * Description:	This function returns true if the given quark is in the
 *		given quarklist.
 *
 * In Args:	
 *		NrmQuarkList	qlist,	NrmNULLQUARK terminated quark list
 *		NrmQuark	q	quark to search for in list
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlBoolean
 * Side Effect:	
 */
NhlBoolean
NrmQinQList
#if	NhlNeedProto
(
	NrmQuarkList	qlist,	/* NrmNULLQUARK terminated quark list	*/
	NrmQuark	q	/* quark to search for in list		*/
)
#else
(qlist,q)
	NrmQuarkList	qlist;	/* NrmNULLQUARK terminated quark list	*/
	NrmQuark	q;	/* quark to search for in list		*/
#endif
{
	while(*qlist != NrmNULLQUARK){
		if(*qlist == q) return True;
		if(*qlist > q) return False;
		qlist++;
	}

	return False;
}

void NrmParseCommand
#if	NhlNeedProto
(
    NrmDatabase		*pdb,		/* data base */
    register NrmOptionDescList options, /* pointer to table of valid options */
    Const char		*prefix,	/* name to prefix resources with     */
    int			*argc,		/* address of argument count 	     */
    char		**argv		/* argument list (command line)	     */
)
#else
(pdb, options, num_options, prefix, argc, argv)
    NrmDatabase		*pdb;		/* data base */
    register NrmOptionDescList options; /* pointer to table of valid options */
    char		*prefix;	/* name to prefix resources with     */
    int			*argc;		/* address of argument count 	     */
    char		**argv;		/* argument list (command line)	     */
#endif
{
    int 		foundOption;
    char		**argsave;
    register int	i, myargc;
    NrmBinding		bindings[100];
    NrmQuark		quarks[100];
    NrmBinding		*start_bindings;
    NrmQuark		*start_quarks;
    char		*optP, *argP, optchar, argchar;
    int			matches;
    enum {DontCare, Check, NotSorted, Sorted} table_is_sorted;
    char		**argend;
    int			num_options=0;
    NrmOptionDescList	opt = options;

#define PutCommandResource(value_str)				\
    {								\
    NrmStringToBindingQuarkList(				\
	options[i].specifier, start_bindings, start_quarks);    \
    NrmQPutStringResource(pdb, bindings, quarks, value_str);    \
    } /* PutCommandResource */

    while(opt->option){
	num_options++;
	opt++;
    }
    myargc = (*argc); 
    argend = argv + myargc;
    argsave = ++argv;

    /* Initialize bindings/quark list with prefix (typically app name). */
    quarks[0] = NrmStringToName(prefix);
    bindings[0] = NrmBindTightly;
    start_quarks = quarks+1;
    start_bindings = bindings+1;

    table_is_sorted = (myargc > 2) ? Check : DontCare;
    for (--myargc; myargc > 0; --myargc, ++argv) {
	foundOption = False;
	matches = 0;
	for (i=0; i < num_options; ++i) {
	    /* checking the sort order first insures we don't have to
	       re-do the check if the arg hits on the last entry in
	       the table.  Useful because usually '=' is the last entry
	       and users frequently specify geometry early in the command */
	    if (table_is_sorted == Check && i > 0 &&
		strcmp(options[i].option, options[i-1].option) < 0) {
		table_is_sorted = NotSorted;
	    }
	    for (argP = *argv, optP = options[i].option;
		 (optchar = *optP++) &&
		 (argchar = *argP++) &&
		 argchar == optchar;);
	    if (!optchar) {
		if (!*argP ||
		    options[i].argKind == NrmoptionStickyArg ||
		    options[i].argKind == NrmoptionIsArg) {
		    /* give preference to exact matches, StickyArg and IsArg */
		    matches = 1;
		    foundOption = i;
		    break;
		}
	    }
	    else if (!argchar) {
		/* may be an abbreviation for this option */
		matches++;
		foundOption = i;
	    }
	    else if (table_is_sorted == Sorted && optchar > argchar) {
		break;
	    }
	    if (table_is_sorted == Check && i > 0 &&
		strcmp(options[i].option, options[i-1].option) < 0) {
		table_is_sorted = NotSorted;
	    }
	}
	if (table_is_sorted == Check && i >= (num_options-1))
	    table_is_sorted = Sorted;
	if (matches == 1) {
		i = foundOption;
		switch (options[i].argKind){
		case NrmoptionNoArg:
		    --(*argc);
		    PutCommandResource(options[i].value);
		    break;
			    
		case NrmoptionIsArg:
		    --(*argc);
		    PutCommandResource(*argv);
		    break;

		case NrmoptionStickyArg:
		    --(*argc);
		    PutCommandResource(argP);
		    break;

		case NrmoptionSepArg:
		    if (myargc > 1) {
			++argv; --myargc; --(*argc); --(*argc);
			PutCommandResource(*argv);
		    } else
			(*argsave++) = (*argv);
		    break;
		
		case NrmoptionResArg:
		    if (myargc > 1) {
			++argv; --myargc; --(*argc); --(*argc);
			NrmPutLineResource(pdb, *argv);
		    } else
			(*argsave++) = (*argv);
		    break;
		
		default:
		    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"NrmParseCommand:Unable to parse \"%s\" (%s)",
				options[i].option,options[i].specifier));
		case NrmoptionSkipArg:
		    if (myargc > 1) {
			--myargc;
			(*argsave++) = (*argv++);
		    }
		    (*argsave++) = (*argv); 
		    break;

		case NrmoptionSkipLine:
		    for (; myargc > 0; myargc--)
			(*argsave++) = (*argv++);
		    break;

		case NrmoptionSkipNArgs:
		    {
			register long j = 1 + (long) options[i].value;

			if (j > myargc) j = myargc;
			for (; j > 0; j--) {
			    (*argsave++) = (*argv++);
			    myargc--;
			}
			argv--;		/* went one too far before */
			myargc++;
		    }
		    break;

		}
	}
	else
	    (*argsave++) = (*argv);  /*compress arglist*/ 
    }

    if (argsave < argend)
	(*argsave)=NULL; /* put NULL terminator on compressed argv */
}
