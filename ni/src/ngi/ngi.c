/*
 *      $Id: ngi.c,v 1.3 1997-01-03 01:37:52 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ngi.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jul 30 08:27:17 MDT 1996
 *
 *	Description:	
 */

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/ShellP.h>
#include <Xm/VendorSP.h>

#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/AppI.h>

#include <ncarg/ngo/xapp.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/mwin.h>
#include <ncarg/ngo/ncledit.h>

/*
 * commandline and fallbacks need to be added to the app object in the
 * hlu library.
 */
static NhlString resdb[] = {
#ifndef	DEBUG
#include "ngi.res.h"
#endif
	NULL
};

static NrmOptionDescRec clineopts[] = {
	{"-noopt","*noopt",NrmoptionSepArg,NULL},
	NULL
};

void
main
#if	NhlNeedProto
(
	int	argc,
	char	**argv
)
#else
(argc,argv)
	int	argc;
	char	**argv;
#endif
{
	int		appid,nxapp,ncl,ne,mn;

	NhlInitialize();

	NhlVACreate(&appid,"ngi",NhlappClass,NhlDEFAULT_APP,
		_NhlNappResourceStrings,	resdb,
		_NhlNappCommandLineOpts,	clineopts,
		_NhlNappArgcInOut,		&argc,
		_NhlNappArgvInOut,		argv,
		NULL);

	/*
	 * This object initializes things in the hlu library so every
	 * object created after it gets its id in the _NhlNguiData.
	 */
	NhlVACreate(&nxapp,"ngi",NgxappMgrClass,appid,
		NgNappName,			"ngi",
		NgNappClass,			"Ngi",
		NgNxappArgc,			argc,
		NgNxappArgv,			argv,
		NULL);

	/*
	 * This object assigns a field in the nxapp object to
	 * its id, so every object has access to it as well.
	 */
	NhlVACreate(&ncl,"nclstate",NgnclStateClass,appid,
		NULL);

#if	NOT
	NhlVACreate(&ne,"ncledit",NgnclEditClass,appid,
		NULL);

	NgGOPopup(ne);
#endif

	/*
	 * Now create the main window object.
	 */

	NhlVACreate(&mn,"main",NgmWinClass,appid,
		NULL);

	NgGOPopup(mn);

	/*
	 * Now, let the application run.
	 */
	NgAppRun(nxapp);
}
