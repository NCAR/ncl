/* 
 * $Id: file.h,v 1.5 2008-07-27 03:22:37 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#ifndef	_file_
#define	_file_

#include "idt.h"

extern	char	*GetFiles(
#ifdef	NeedFuncProto
	char	*file_filter,
	int	*longest
#endif
);

extern	void SetFileSelection(
#ifdef	NeedFuncProto
	char	*file,
	FuncPtrPasser	select_action
#endif
);
	
extern	char	*GetFileSelection();

#endif	/*	_file_	*/
