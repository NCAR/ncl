/*
 *      $Id: ListSupport.h,v 1.2 2000-01-28 23:31:24 ethan Exp $
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

extern int _NclGetNext(struct _NclObjRec *);


extern struct _NclListRec* _NclListSelect(struct _NclListRec *, NclSelection *);

extern NhlErrorTypes _NclListPush(NclObj , NclObj );

extern struct _NclObjRec* _NclListPop(NclObj );

extern NhlErrorTypes _NclListSetType(NclObj , int );


#endif /*_ListSupport_h */


