/*
 *      $Id: ngi.c,v 1.9 1998-08-26 22:48:28 dbrown Exp $
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
#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/xwk.h>

static NhlString resdb[] = {
#ifndef	RESDEBUG
#include "ngi.res.h"
#endif
	NULL
};

static NrmOptionDescRec clineopts[] = {
	{"-noopt","*noopt",NrmoptionSepArg,NULL},
	NULL
};

void CreateFirstXWork(
	int nxapp,
	int ncl
)
{
	char		*name;
	char		line[512];

	/*
	 * Create the first X window -- not popped up though
	 */

	name = NgNclGetSymName(ncl,"Xwk",True);
	sprintf(line,
	"%s = create \"%s\" xWorkstationClass defaultapp\nend create\n",
		name,name);
	(void)NgNclSubmitBlock(ncl,line);

	NgAppSetSelectedWork(nxapp,name);
}

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
	int		appid,nxapp,ncl,ne,browse,mn,xwk;

	NhlInitialize();

	NhlVACreate(&appid,"ngi",NhlappClass,NhlDEFAULT_APP,
		NhlNappDefaultParent,		True,
		_NhlNappResourceStrings,	resdb,
		_NhlNappCommandLineOpts,	clineopts,
		_NhlNappArgcInOut,		&argc,
		_NhlNappArgvInOut,		argv,
		NULL);

	/*
	 * This object initializes things in the hlu library so every
	 * object created after it gets its id in the _NhlNguiData.
	 */
	NhlVACreate(&nxapp,"NgNGO",NgxappMgrClass,appid,
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

	/*
	 * Create an Ncleditor object - don't pop it up.  Just make
	 * sure one is created immediately, so it records all ncl
	 * commands that happen.
	 */
	NhlVACreate(&ne,"ncledit",NgnclEditClass,appid,NULL);
	NgGOCreateWindow(ne);

	NhlVACreate(&browse,"browse",NgbrowseClass,appid,NULL);
	NgGOCreateWindow(browse);

	CreateFirstXWork(nxapp,ncl);

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
