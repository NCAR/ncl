

/*
 *      $Id: NclAtt.h,v 1.3 1996-07-16 20:58:16 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jan 13 15:04:41 MST 1994
 *
 *	Description:	
 */
#ifndef NclAtt_h
#define NclAtt_h
#include "NclData.h"

typedef struct _NclAttRec *NclAtt;
typedef struct _NclAttClassRec *NclAttClass;

typedef NhlErrorTypes (*NclAddAttFunction) (
#if 	NhlNeedProto
NclAtt /*theattobj*/,
char *  /*attname*/,
NclMultiDValData  /*value*/,
NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef struct _NclMultiDValDataRec * (*NclGetAttFunction) (
#if NhlNeedProto
NclAtt /*theattobj*/,
char *  /*attname*/,
NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef int (*NclIsAttFunction) (
#if NhlNeedProto
NclAtt	/*theattobj*/,
char *  /*attname*/
#endif
);

typedef void (*NclDelAttFunction) (
#if  NhlNeedProto
NclAtt	/* theattobj*/,
char 	* /*attname*/
#endif
);

typedef void (*NclAttPrintFunction)(
#if  NhlNeedProto
NclAtt /*theattobj*/,
FILE * /*fp*/
#endif
);

typedef struct _NclAttRec * (*NclCopyAttFunction) (
#if   NhlNeedProto
NclAtt /*theattobj*/,
NclAtt /*storage*/
#endif
);

extern int _NclAttCreate(
#if	NhlNeedProto
	struct _NclObjRec *	/* int */,
	struct _NclObjClassRec *	/* theclass */,
	NclObjTypes 	/* obj_type */,
	unsigned int 	/* obj_type_mask */,
	struct _NclObjRec *	/* parent */
#endif
);

typedef struct _NclAttList {
	int	quark;
	char	*attname;
	NclMultiDValData attvalue;
	_NhlCB	cb;
	struct _NclAttList *next;
}NclAttList;

typedef struct _NclAttClassPart {
	NclAddAttFunction	add_att;
	NclGetAttFunction	get_att;
	NclDelAttFunction	del_att;
	NclIsAttFunction	is_att;
	NclCopyAttFunction	copy_att;
} NclAttClassPart;


typedef struct _NclAttPart {
	int				n_atts;
	NclAttList			*att_list;
}NclAttPart;
 
typedef struct _NclAttClassRec{
	NclObjClassPart	obj_class;
	NclAttClassPart att_class;
}NclAttClassRec;

typedef struct _NclAttRec {
	NclObjPart      obj;
	NclAttPart	att;
}NclAttRec;


extern NclObjClass nclAttClass;

extern NclAttClassRec nclAttClassRec;

#endif /* NclAtt_h */
