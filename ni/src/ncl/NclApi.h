
/*
 *      $Id: NclApi.h,v 1.1 1993-09-24 23:40:47 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		NclApi.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Aug 17 13:43:10 MDT 1993
 *
 *	Description:	
 */
#ifndef _NCNclApi_h
#define _NCNclApi_h


extern int NclInitServer(
#ifdef NhlNeedProto
FILE * /* error_file */,
NhlErrorTypes /*error_level
#endif
);

extern void NclCloseServer(
#ifdef NhlNeedProto
void
#endif
);

extern int NclSubmitBlock1(
#ifdef NhlNeedProto
	char *	/*script*/,
	int	/*script_size*/
#endif
);

extern int NclSubmitBlock2(
#ifdef NhlNeedProto
	char *script[]
#endif
);

extern int NclSubmitCommand(
#ifdef NhlNeedProto
	char * /*command*/
#endif
);

extern void NclPrintErrorMsgs(
#ifdef NhlNeedProto
	void
#endif
);

extern int NclGetErrorId(
#ifdef NhlNeedProto
void
#endif
);

#endif  /*_NCNclApi_h */
