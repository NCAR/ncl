/*
 *      $Id: filetreeP.h,v 1.2 1997-06-06 03:14:52 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		filetreeP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 14:22:58 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_FILETREEP_H_
#define	_NG_FILETREEP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/filetree.h>

 
#define DEBUG_FILETREE 0

/* defines for each node of the file tree
 * -- not including the leaf nodes.
 * 10's digit represents level in the tree hierarchy
 */
#define _ftTop		0

#define _ftInfo 	10
#define _ftAttr 	11
#define _ftDim		12
#define _ftVar		13

#define _ftLAttr	20
#define _ftLDim		21
#define _ftLVar		22

#define _ftDInfo	30
#define _ftDAttr	31
#define _ftVInfo	32
#define _ftVDim		33
#define _ftVAttr	34

#define _ftLDAttr	40
#define _ftLVDim	41
#define _ftLVAttr	42

#define _ftVDInfo	50
#define _ftVDAttr	51

#define _ftLVDAttr	60

typedef char _ftNodeType;
typedef char _ftBool;


typedef struct _ftNodeData 
{
        struct _ftNodeData *parent;
        NrmQuark	qname;
        _ftNodeType	type;
        _ftBool		expanded;
        unsigned short	subcount;
	struct _ftNodeData  *subdata;
} ftNodeData;
        

/*
 * Any field in the public structure NgFileTree, defined in
 * filetree.h, must appear in the same order at the beginning
 * of the NgFileTreeRec definition. 
 */

typedef struct _NgFileTreeRec 
{
            /* public fields - exported as NgFileTree */
        
        Widget			tree;
        ftGeoNotifyFunc		geo_notify;
        NhlPointer		geo_data;
        
            /* private fields */
        NgGO			go;
        NrmQuark		qfileref;
 	NclApiDataList		*dlist;
        Dimension		c2_width;
        NhlBoolean		created;
        NhlBoolean		expand_called;
        ftNodeData		file;
} NgFileTreeRec;

#endif	/* _NG_FILETREEP_H_ */
