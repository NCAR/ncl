/*
 *	$Id: devices.h,v 1.7 1992-02-29 00:13:43 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*
 *	devices.h
 *
 *	
 *      Author          John Clyne      (clyne@redcloud.ucar.edu)
 *
 *      Date            Mon Nov 28 14:45:24 MST 1988
 *
 *              This file contains the devices ctrans supports. If a new
 *	devices is to be added this file and devices.c must be modified
 */

#include <ncarv.h>
#define	GCAP	/* GCAP must always be defined	*/

/*
 *	The following allows us to use the C preprocessor to calculate
 *	the number of devices ctrans is being compiled with. If a new
 *	device is to be added it must first be #defined by a unique name
 *	such as X11. In order for us to use cpp we must also #define a 
 *	dummy, such as X in the case of X11, that has the value 1 if the
 *	named devise is defined else it is zero.
 */
#ifdef	GCAP
#define	G	1
#define	R	1
#else
#define	G	0
#define	R	0
#endif	

#ifdef	X11
#define	X	1
#else
#define	X	0
#endif	

#ifdef	CTXT
#define	CL	1
#else
#define	CL	0
#endif	

#ifdef	SunV
#define	SV	1
#else
#define	SV	0
#endif

#ifdef	SunV
#define	SR	1
#else
#define	SR	0
#endif


/*
 *	These #defines determine a index using cpp into the device 
 *	table for a particular device. For each new device added 
 *	a new unique index for that device must be defined. The new
 *	index is defined by summing the dummy defines that *preceed* it
 *	in this file with the new devices dummy define. Order is 
 *	important.
 */
#define	GCAP_I	(G -1)
#define	RAST_I	(G + R -1)
#define	X11_I	(G + R + X -1)
#define	CTXT_I	(G + R + X + CL -1)
#define	SunV_I	(G + R + X + CL + SV -1)
#define	SunR_I	(G + R + X + CL + SV + SR -1)


/*
 *      A static structure to relate device names to index numbers.
 *      This needs to be expanded when a new device is added. The
 *	new device must be added in a similar manner to the devices 
 *	that already exist substituting the defined index for 
 *	device.number and defined device name for device.name.
 */

#ifdef	DEVICES

static	OptDescRec	gcap_opts[] = {
	{"window", OptSepArg, NULL},
	{"viewport", OptSepArg, NULL},
	{NULL}
	};

static	OptDescRec	raster_opts[] = {
	{"resolution", OptSepArg, "512x512"},
	{"window", OptSepArg, NULL},
	{"viewport", OptSepArg, NULL},
	{"compress", OptIsArg, "false"},
	{"landscape", OptIsArg, "false"},
	{"rle", OptIsArg, "false"},
	{"dpi", OptSepArg, "75"},
	{NULL}
	};

#ifdef	X11
static	OptDescRec	X11_opts[] = {
	{"geometry", OptSepArg, NULL},
	{"window", OptSepArg, NULL},
	{"viewport", OptSepArg, NULL},
	{"foreground", OptSepArg, NULL},
	{"background", OptSepArg, NULL},
	{"reverse", OptIsArg, "false"},
	{NULL}
	};
#endif

#ifdef	CTXT
static	OptDescRec	CTXT_opts[] = {
	{"Data", OptIsArg, "true"},
	{"Para", OptIsArg, "true"},
	{NULL}
	};
#endif

#if 	defined(SunV) || defined(SunR)
static	OptDescRec	SunV_opts[] = {
	{"Ws", OptSepArg, "-1 -1"},
	{"Wp", OptSepArg, "0 0"},
	{NULL}
	};
#endif


struct device{
        char    *name;		/* name of device			*/
        int     number;		/* number of device			*/
        boolean usegcap;	/* true if device uses a graphcap	*/
        boolean usefcap;	/* true if device uses a fontcap	*/
        char    *gcapname;	/* unused				*/
        boolean    use_common;	/* use common device interface		*/
	OptDescRec	*opt;	/* command line options for the device	*/
} devices[] = {
        {"gcap",GCAP_I,TRUE,TRUE,"", TRUE, gcap_opts}
        ,{"xwd",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"nrif",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"hdf",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"sun",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"parallax",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}

#ifdef  X11
        ,{"X11",X11_I,FALSE,TRUE,"", FALSE, X11_opts}
#endif   

#ifdef  CTXT
        ,{"CTXT",CTXT_I,FALSE,FALSE,"", FALSE, CTXT_opts}
#endif   

#ifdef  SunV
        ,{"sunview",SunV_I, FALSE,TRUE,"", FALSE, SunV_opts}
        ,{"sunraster",SunR_I, FALSE,TRUE,"", FALSE, SunV_opts}
#endif   

};


#else

extern	struct device{
        char    *name;
        int     number;
        boolean usegcap;
        boolean usefcap;
        char    *gcapname;
        boolean    use_common;	/* use common device interface		*/
	OptDescRec	*opt;	/* command line options for the device	*/
} devices[];

extern	int	devicenum;	/* number of defined devices	*/
#endif	
