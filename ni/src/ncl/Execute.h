
/*
 *      $Id: Execute.h,v 1.1 1993-12-21 19:17:31 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Execute.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Oct 14 12:41:56 MDT 1993
 *
 *	Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
			Ncl_ERRORS, 
			Ncl_STOPS, 
			Ncl_BREAKS, 
			Ncl_CONTINUES }NclExecuteReturnStatus ;


NclExecuteReturnStatus _NclExecute(
#if NhlNeedProto
	unsigned long start_offset
#endif
);
#ifdef __cpluspus
}
#endif
