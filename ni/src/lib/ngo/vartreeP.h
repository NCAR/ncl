/*
 *      $Id: vartreeP.h,v 1.1 1997-06-04 18:08:37 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		vartreeP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 24 14:37:14 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_VARTREEP_H_
#define	_NG_VARTREEP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/vartree.h>

 
#define DEBUG_ENTRY 1
#define DEBUG_TREE 1 << 1
#define DEBUG_VARTREE DEBUG_ENTRY | DEBUG_TREE

/* defines for each node of the file tree
 * -- not including the leaf nodes.
 * 10's digit represents level in the tree hierarchy
 */
#define _vtTop		0

#define _vtVInfo	10
#define _vtVDim		11
#define _vtVAttr	12

#define _vtLVDim	20
#define _vtLVAttr	21

#define _vtVDInfo	30
#define _vtVDAttr	31

#define _vtLVDAttr	40

typedef char _vtNodeType;
typedef char _vtBool;


typedef struct _vtNodeData 
{
        struct _vtNodeData *parent;
        NrmQuark	qname;
        _vtNodeType	type;
        _vtBool		expanded;
        unsigned short	subcount;
	struct _vtNodeData  *subdata;
} vtNodeData;
        

/*
 * Any field in the public structure NgVarTree, defined in
 * vartree.h, must appear in the same order at the beginning
 * of the NgVarTreeRec definition. 
 */

typedef struct _NgVarTreeRec 
{
            /* public fields - exported as NgVarTree */
        
        Widget			tree;
        vtGeoNotifyFunc		geo_notify;
        NhlPointer		geo_data;
        
            /* private fields */
        NgGO			go;
        NrmQuark		qfileref;
        NrmQuark		qvar;
 	NclApiDataList		*dlist;
        Dimension		c2_width;
        NhlBoolean		created;
        NhlBoolean		expand_called;
        vtNodeData		var;
} NgVarTreeRec;

#endif	/* _NG_VARTREEP_H_ */
