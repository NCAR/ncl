
/*
 *      $Id: AddBuiltIns.c,v 1.1 1993-12-21 19:17:26 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AddBuiltIns.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 15 10:43:07 MDT 1993
 *
 *	Description:	
 */
#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <data_objs/NclData.h>
#include <defs.h>
#include <y.tab.h>
#include <Symbol.h>
#include <ProcFuncs.h>

extern void _NclPrint(
#ifdef NhlNeedProto
void
#endif 
);

void _NclAddBuiltIns
#if  __STDC__
(void)
#else 
()
#endif
{
	NclArgTemplate *args;
	int i;

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 0;
	_NclRegisterBuiltInProc(_NclPrint,args,"print",1);
}


#ifdef __cpluplus
}
#endif
