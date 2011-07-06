/*
 *      $Id: ListSupport.h,v 1.4 2004-04-28 17:02:12 grubin Exp $
 */
/************************************************************************
*                                                                       *
*                            Copyright (C)  1993                        *
*            University Corporation for Atmospheric Research            *
*                            All Rights Reserved                        *
*                                                                       *
************************************************************************/
/*
 *      File:
 *
 *      Author:         Ethan Alpert
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Mon Sep 20 16:23:40 MDT 1999
 *
 *      Description:
 */

#ifndef _ListSupport_h
#define _ListSupport_h


extern struct _NclListRec* _NclListSelect(struct _NclListRec *, NclSelection *);

extern NhlErrorTypes _NclListPush(NclObj , NclObj );

extern struct _NclObjRec* _NclListPop(NclObj );

extern NhlErrorTypes _NclListSetType(NclObj , int );

extern int _NclListGetType(NclObj );

extern int _NclListGetNext(NclObj );

extern void _NclListDestroy(NclObj);

extern NhlErrorTypes _NclListAppend(NclObj , NclObj );

#endif /*_ListSupport_h */


