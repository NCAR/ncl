/*
 *      $Id: AttSupport.h,v 1.2 1996-04-23 00:10:09 ethan Exp $
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
 *	Date:		Wed Jul 13 13:51:32 MDT 1994
 *
 *	Description:	
 */
#ifndef _AttSupport_h
#define _AttSupport_h

extern struct _NclAttRec * _NclCopyAtt(
#if     NhlNeedProto
struct _NclAttRec * /*theattobj*/,
struct _NclAttRec * /*storage*/
#endif
);

extern NhlErrorTypes _NclAddAtt(
#if NhlNeedProto
int /*id*/,
char * /*attname*/,
struct _NclMultiDValDataRec *  /*value*/,
NclSelectionRecord * /*sel_ptr*/
#endif
);

extern int _NclIsAtt(
#if NhlNeedProto
int /*id*/,
char * /* name*/
#endif
);

extern struct _NclMultiDValDataRec* _NclGetAtt(
#if NhlNeedProto
int /*id*/,
char * /*attname*/,
NclSelectionRecord * /*sel_ptr*/
#endif
);

extern void _NclDeleteAtt(
#if NhlNeedProto
int /*id*/,
char * /*attname*/
#endif
);

extern void _NclDeleteAttMDID(
#if NhlNeedProto
int /*id*/,
int /*md_id*/
#endif
);

#endif /* _AttSupport_h */
