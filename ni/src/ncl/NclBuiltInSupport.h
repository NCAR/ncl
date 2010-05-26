
/*
 *      $Id: NclBuiltInSupport.h,v 1.2.22.1 2008-03-28 20:37:49 grubin Exp $
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
 *	Date:		Tue Jan 31 15:14:25 MST 1995
 *
 *	Description:	
 */
#ifndef Ncl_BUILTINSUPPORT_h
#define Ncl_BUILTINSUPPORT_h

extern void *NclGetArgValue(
#if NhlNeedProto
int /*arg_num*/, int /*n_args*/,int* /*n_dims*/, ng_size_t* /*dimsizes*/, NclScalar* /*missing*/, int * /*has_missing*/, NclBasicDataTypes * /*type*/, int /* access_type */
#endif
);

extern NhlErrorTypes NclReturnValue(
#if NhlNeedProto
void *value, int /*n_dims*/, ng_size_t* /*dimsizes*/, NclScalar* /*missing*/, NclBasicDataTypes /*type*/, int /*copy_data*/
#endif
);

#endif  /*Ncl_BUILTINSUPPORT_h*/
