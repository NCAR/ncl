/*
 *      $Id: ngi.c,v 1.1 1996-10-10 18:55:51 boote Exp $
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
#include <ncarg/hlu/App.h>

#include <ncarg/ngo/xapp.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/ncledit.h>

/*
 * commandline and fallbacks need to be added to the app object in the
 * hlu library.
 */
#if	NOT

static String fallback[] = {
	"ngi.version:		NOTFOUND",
	NULL
};

static XrmOptionDescRec clineopts[] = {
	{"-noopt","*noopt",XrmoptionSepArg,NULL}
};

#endif

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
	int		appid,nxapp,ncl,ne;

	NhlInitialize();
	NhlVACreate(&appid,"ngi",NhlappClass,NhlDEFAULT_APP,
#if	NOT
		NgNappFallback,			fallback,
		NgNappCommandLineOpts,		clineopts,
		NgNappNumCommandLineOpts,	XtNumber(clineopts),
#endif
		NULL);

	NhlVACreate(&nxapp,"ngi",NgxappMgrClass,appid,
		NgNappName,			"ngi",
		NgNappClass,			"Ngi",
		NgNxappArgc,			argc,
		NgNxappArgv,			argv,
		NULL);

	NhlVACreate(&ncl,"nclstate",NgnclStateClass,appid,
		NgNAppMgr,	nxapp,
		NULL);

	NhlVACreate(&ne,"ncledit",NgnclEditClass,appid,
		NgNAppMgr,	nxapp,
		NgNNclState,	ncl,
		NULL);

	NgGOPopup(ne);
#if	NOT
	NhlVACreate(&ne,"ncledit",NgnclEditClass,appid,
		NgNAppMgr,	nxapp,
		NgNNclState,	ncl,
		NULL);

	NgGOPopup(ne);
#endif

	/*
	 * Now create the main window object.
	 */

	/*
	 * Now, let the application run.
	 */
	NgAppRun(nxapp);
}
