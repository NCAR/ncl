
/*
 *      $Id: NclBuiltIns.h,v 1.1.24.1 2008-03-28 20:37:49 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
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
 *	Date:		Tue Jan 31 15:17:59 MST 1995
 *
 *	Description:	
 */
#ifndef Ncl_BUILTINS_h
#define Ncl_BUILTINS_h

typedef NhlErrorTypes (*NclPubBuiltInFuncWrapper)(
#if     NhlNeedProto
        void
#endif
);
typedef NhlErrorTypes (*NclPubBuiltInProcWrapper)(
#if     NhlNeedProto
        void
#endif
);


extern void *NewArgs(
#if NhlNeedProto
int /* n */
#endif
);
extern void SetArgTemplate(
#if NhlNeedProto
void * /*args*/,
int 	/* arg_num*/,
char * 	/* type_name*/,
int 	/* n_dims*/, 
ng_size_t *	/*dimsizes*/
#endif
);
extern void NclRegisterProc(
#if NhlNeedProto
NclPubBuiltInProcWrapper   /* thefuncptr */,
void*	/* args */,
char*			/* fname*/,
int			/* n_args */
#endif
);
extern void NclRegisterFunc(
#if NhlNeedProto
NclPubBuiltInFuncWrapper   /* thefuncptr */,
void *	/* args */,
char*			/* fname*/,
int			/* n_args */
#endif
);

#endif /* Ncl_BUILTINS_h */
