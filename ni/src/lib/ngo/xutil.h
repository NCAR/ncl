/*
 *      $Id: xutil.h,v 1.2 1996-11-24 22:27:36 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xutil.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Oct 8 16:43:13 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_XUTIL_H
#define	_NG_XUTIL_H
#include <ncarg/ngo/nclstate.h>

#include <Xm/Xm.h>

typedef NhlBoolean (*NgXListTest)(
	NgNclObj	node,
	NhlPointer	udata
);

typedef struct NgXListNodeRec NgXListNodeRec, *NgXListNode;
struct NgXListNodeRec{
	int		pos;
	int		id;	/* id for hlu obj's - quark vname for vars */

	Const char	*name;	/* key hlu obj name, vname */
	XmString	xmname;

	NgXListNode	left;
	NgXListNode	right;
};

extern NgXListNode NgXListGetByPos(
	Widget	xmlist,
	int	pos
);

extern NgXListNode NgXListGetByID(
	Widget	xmlist,
	int	id
);

#if	TODOIFNEEDED
extern NgXListNode NgXListGetByName(
	Widget	xmlist,
	char	*name
);
#endif

/*
 * NgXListManage
 * ltype must be one of:
 *		NgNclHLUOBJ,
 *		NgNclHLUVAR,
 *		NgNclVAR,
 *		NgNclFILEVAR,
 *		NgNclFUNC
 * if tfunc is NULL all object of that "type" will be included in list
 */
extern NhlBoolean NgXListManage(
	int		nsid,	/* nclstate obj id		*/
	Widget		xmlist,	/* XmListWidget to hold list	*/
	NgNclCBType	ltype,	/* list type			*/
	NgXListTest	tfunc,	/* T/F test for inclusion	*/
	NhlPointer	closure	/* sent to tfunc with NgNclObj	*/
);

extern void NgXFileSearchProc(
	Widget		w,
	XtPointer	cbdata
);

#endif	/* _NG_XUTIL_H */
