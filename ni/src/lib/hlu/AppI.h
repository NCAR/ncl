/*
 *      $Id: AppI.h,v 1.1 1994-08-11 21:36:52 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AppI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jul 29 16:42:28 MDT 1994
 *
 *	Description:	
 */
#ifndef	_NAppI_h
#define	_NAppI_h

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/App.h>

#define	_NhlNappMode	"app.Mode"
#define	_NhlCappMode	_NhlClangMode

#define	_NhlNnoAppDB	"no.App.DB"
#define	_NhlCnoAppDB	"No.App.DB"

#define	_NhlNdefApp	"def.App"
#define	_NhlCdefApp	"Def.App"

extern	NhlLayer _NhlGetCurrentApp(
#if	NhlNeedProto
	void
#endif
);

extern	NrmDatabase _NhlGetBaseDB(
#if	NhlNeedProto
	NhlLayer	l
#endif
);

extern	NrmDatabase _NhlGetAppDB(
#if	NhlNeedProto
	NhlLayer	l
#endif
);

extern void _NhlSetLang(
#if	NhlNeedProto
	_NhlC_OR_F	ltype
#endif
);

#endif	/* _NAppI_h */
