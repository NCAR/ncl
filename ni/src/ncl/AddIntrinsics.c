

/*
 *      $Id: AddIntrinsics.c,v 1.1 1994-03-03 23:41:18 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AddIntrinsics.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 7 11:07:41 MST 1994
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

extern void _NclIPrint(
#ifdef NhlNeedProto
void
#endif 
);
extern void _NclIDelete(
#ifdef NhlNeedProto
void
#endif
);

void _NclAddIntrinsics
#if  __STDC__
(void)
#else 
()
#endif
{
	NclArgTemplate *args;

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIPrint,args,"print",1,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIDelete,args,"delete",1,IPROC);
	return;
}


#ifdef __cpluplus
}
#endif
