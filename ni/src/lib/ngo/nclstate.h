/*
 *      $Id: nclstate.h,v 1.3 1997-06-20 16:35:35 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		nclstate.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Aug 30 10:56:48 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_NCLSTATE_H
#define	_NG_NCLSTATE_H

#include <ncarg/hlu/Base.h>
#include <ncarg/ngo/ngo.h>

extern NhlClass NgnclStateClass;

/*
 * Resources
 */

/*
 * Callbacks
 */
#define NgCBnsObject	"cb.nsObject"	/* cbdata.ptrval is NgNclObj */
#define NgCBnsOutput	"cb.nsOutput"	/* cbdata.strval is output */
#define NgCBnsErrOutput	"cb.nsErrOutput"/* cbdata.ptrval is *NhlErrMsg */
#define NgCBnsSubmit	"cb.nsSubmit"	/* cbdata.strval is submitted */
#define NgCBnsPrompt	"cb.nsPrompt"	/* cbdata.ptrval=NgNclPromptCBData */
#define NgCBnsReset	"cb.nsReset"	/* cbdata is not used	*/

typedef struct _NgNclPromptCBDataRec{
	int		line;
	NhlBoolean	istate;
	NhlBoolean	err;
} NgNclPromptCBDataRec, *NgNclPromptCBData;

typedef enum _NgNclCBType{
	NgNclCBUNKNOWN=0,

	NgNclCBCREATE_HLUOBJ=1,
	NgNclCBCREATE_HLUVAR=2,
	NgNclCBCREATE_VAR=3,
	NgNclCBCREATE_FILEVAR=4,

	NgNclCBDELETE_HLUOBJ=5,
	NgNclCBDELETE_HLUVAR=6,
	NgNclCBDELETE_VAR=7,
	NgNclCBDELETE_FILEVAR=8,

	NgNclCBCREATE_FUNC=9,

	NgNclHLUOBJ=10,
	NgNclHLUVAR=11,
	NgNclVAR=12,
	NgNclFILEVAR=13,
	NgNclFUNC=14
} NgNclCBType;

typedef union _NgNclObj NgNclObjRec, *NgNclObj;

typedef struct {
	NgNclCBType	cbtype;
	NgNclObj	next;
	NgNclObj	left;
	NgNclObj	right;
	NgNclObj	parent;

	Const char	*name;
	int		id;
	int		ref_count;
} NgNclAnyRec, *NgNclAny;

typedef struct {
	NgNclCBType	cbtype;
	NgNclObj	next;
	NgNclObj	left;
	NgNclObj	right;
	NgNclObj	parent;

	Const char	*name;
	int		id;
	int		ref_count;

	int		parent_id;
	NhlClass	class_ptr;

	Const char	*vname;

} NgNclHluObjRec, *NgNclHluObj;

typedef struct {
	NgNclCBType	cbtype;
	NgNclObj	next;
	NgNclObj	left;
	NgNclObj	right;
	NgNclObj	parent;

	Const char	*name;
	int		id;
	int		ref_count;

	NhlBoolean	scalar;
} NgNclHluVarRec, *NgNclHluVar;

typedef struct {
	NgNclCBType	cbtype;
	NgNclObj	next;
	NgNclObj	left;
	NgNclObj	right;
	NgNclObj	parent;

	Const char	*name;
	int		id;
	int		ref_count;

	NhlBoolean	scalar;
} NgNclVarRec, *NgNclVar;

typedef struct {
	NgNclCBType	cbtype;
	NgNclObj	next;
	NgNclObj	left;
	NgNclObj	right;
	NgNclObj	parent;

	Const char	*name;
	int		id;
	int		ref_count;
} NgNclFileVarRec, *NgNclFileVar;

typedef struct {
	NgNclCBType	cbtype;
	NgNclObj	next;
	NgNclObj	left;
	NgNclObj	right;
	NgNclObj	parent;

	Const char	*name;
	int		id;
	int		ref_count;
} NgNclFuncRec, *NgNclFunc;


union _NgNclObj {
	NgNclCBType	cbtype;
	NgNclAnyRec	ngany;
	NgNclHluObjRec	nghlu;
	NgNclHluVarRec	nghluvar;
	NgNclVarRec	ngvar;
	NgNclFileVarRec	ngfile;
	NgNclFuncRec	ngfunc;
};

extern NhlBoolean
NgNclProcessObj(
	int		nclstate,
	NgNclCBType	obj_type,
	int		id
);

extern NhlBoolean
NgNclSubmitLine(
	int		nclstate,
	char		*commad,
	NhlBoolean	reset
);

extern NhlBoolean
NgNclSubmitBlock(
	int		nclstate,
	char		*commad
);

extern int
NgNclCurrentLine(
	int	nclstate
);

extern void
NgNclReset(
	int	nclstate
);

/*
 * The NgNclEnumerateObj will call the "func" for each and every object
 * of type NgNclCBType that is currently in the nclstate list.  There
 * is no defined order to the way they are called, and it is not legal
 * to make any calls that would change the internal list from the
 * "func" (i.e. NgNclProcessObj is NOT valid!).
 */
typedef void (*NgNclEnumFunc)(
	NgNclObj	obj,
	NhlPointer	udata
);

extern void
NgNclEnumerateObj(
	int		nclstate,
	NgNclCBType	otype,
	NgNclEnumFunc	func,
	NhlPointer	udata
);

/*
 * This function finds the first symbol that isn't already being used
 * with the given basename, and an integer appended.
 * This function returns a pointer to internal memory, so don't free it.
 * The internal memory is copied over each time it is called, so copy
 * the result immediately.
 */
extern NhlString
NgNclGetSymName(
	NhlString	basename,
        NhlBoolean	add_zero
);

#endif	/* _NG_NCLSTATE_H */
