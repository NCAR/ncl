/*
 *      $Id: nclstate.h,v 1.10 1999-02-23 03:56:51 dbrown Exp $
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
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ngo/ngo.h>

extern NhlClass NgnclStateClass;

/*
 * Resources
 */

#define NgNnsHluClasses		"nsHluClasses"
#define NgCnsHluClasses		"NsHluClasses"
#define NgNnsHluClassCount	"nsHluClassCount"
#define NgCnsHluClassCount	"NsHluClassCount"

/*
 * Callbacks
 */
#define NgCBnsObject	"cb.nsObject"	/* cbdata.ptrval is NgNclObj */
#define NgCBnsOutput	"cb.nsOutput"	/* cbdata.strval is output */
#define NgCBnsErrOutput	"cb.nsErrOutput"/* cbdata.ptrval is *NhlErrMsg */
#define NgCBnsSubmit	"cb.nsSubmit"	/* cbdata.strval is submitted */
#define NgCBnsPrompt	"cb.nsPrompt"	/* cbdata.ptrval=NgNclPromptCBData */
#define NgCBnsReset	"cb.nsReset"	/* cbdata is not used	*/
#define NgCBnsPostSubmit	"cb.nsPostSubmit"	/* cbdata not used */

typedef struct _NgNclPromptCBDataRec{
	int		line;
	NhlBoolean	istate;
	NhlBoolean	err;
} NgNclPromptCBDataRec, *NgNclPromptCBData;

/*
 * This constant is used as the difference from NgNclCBCREATE_{} to
 * NgNclCBDELETE_{} so ORDER and NUMERICAL values in the _NgNclCBType
 * ARE IMPORTANT!
 */
#define	NgNclCB_C_D_DIFF	(NgNclCBDELETE_HLUOBJ-NgNclCBCREATE_HLUOBJ)

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
	int		nclstate,
	NhlString	basename,
        NhlBoolean	add_zero
);

extern NhlString
NgNclGetHLURef(
	int		nclstate,
	int		hluid
);

/*
 * This function returns a single int id for the first Hlu object represented
 * by the ncl symbol name. If the variable is scalar then the id_array
 * output parameter will be set to NULL. Otherwise it will be the array 
 * of Hlu Ids and will need to be freed by the caller.
 * If the variable is of graphic type but no hlu object exists, NHlNULLOBJID
 * will be returned. If the variable does not exist or is not of graphic type
 * a warning is issued and the return value will be NhlWARNING.
 */
 
extern int
NgNclGetHluObjId(
	int		nclstate,
	NhlString	hlu_varname,
        int		*count,
        int		**id_array
        );

/*
 * Given the class name of a user-instantiable Hlu class, returns the
 * class pointer. Returns NULL if the name does not match a valid class.
 */

extern NhlClass
NgNclHluClassPtrFromName(
        int		nclstate,
	NhlString	hlu_classname
        );

typedef enum _NgGraphicType 
{
        NgWORKSTATION,NgDATAITEM,NgTRANSFORM,NgVIEW,
        NgANNOMANAGER,NgSTYLE,NgDATASPEC
} NgGraphicType;

/*
 * returns list of Hlu symbols that represent objects whose class falls into
 * the requested category. If the symbol is an array,
 * it is included in the list if any element belongs to the category.
 * If the count_only value is True, then only the count is returned.
 */
 
extern int
NgNclGetHluSymbols(
        int		nclstate,
        NgGraphicType	type,
        NrmQuark	*qsymbols,
        NhlBoolean	count_only
        );

typedef enum _NgNclBlockType 
{
        _NgCREATE, _NgSETVAL, _NgGETVAL
} NgNclBlockType;


extern int
NgNclVisBlockBegin
(
        int		nclstate,
        NgNclBlockType	type,
        NhlString	ncl_graphic,
        NhlString	ncl_parent,
        NhlString	classname
        );

extern NhlErrorTypes
NgNclVisBlockAddResList
(
        int		nclstate,
        int		block_id,
        int		res_count,
        NhlString	*res_names,
        NhlString	*values,
        NhlBoolean	*quote
        );

extern NhlErrorTypes
NgNclVisBlockEnd
(
        int		nclstate,
        int		block_id
        );

extern NhlErrorTypes
NgNclCopyShapedVar
(
        int		nclstate,
        NhlString       to_varname,
	NrmQuark	qfile,
	NrmQuark	qvar,
	int		ndims,
	long		*start,
	long		*finish,
	long		*stride
);

#endif	/* _NG_NCLSTATE_H */
