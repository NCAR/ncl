/*
 *	$Id: devices.h,v 1.24 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

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

#include <ncarg/c.h>
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
	{"window", 1, NULL, "Specify window transform - llx:lly:urx:ury"},
	{"viewport", 1, NULL, "Specify viewport transform - llx:lly:urx:ury"},
	{"simulatebg", 0, NULL, "Simulate background color with a big polygon"},
	{"outfile", 1,"stdout","Write output to file 'arg0'"},
	{NULL}
	};

static	OptDescRec	raster_opts[] = {
	{"resolution", 1, "512x512", "Raster resolution - widthxheight"},
	{"window", 1, NULL, "Specify window transform - llx:lly:urx:ury"},
	{"viewport", 1, NULL, "Specify viewport transform - llx:lly:urx:ury"},
	{"compress", 0, NULL, "Do compress output image"},
	{"landscape", 0, NULL, "Do landscape mode"},
	{"rle", 0, NULL, "Do run length encode output (if supported)"},
	{"dpi", 1, "150", "Dots per inch - (HPPCL only)"},
	{"direct", 0,NULL,"Do output direct encoded imagery (default indexed)"},
	{"outfile", 1,"stdout","Write output to file 'arg0'"},
	{NULL}
	};

#ifdef	X11
static	OptDescRec	X11_opts[] = {
	{"geometry", 1, NULL, "Window geometry (X11 only)"},
	{"window", 1, NULL, "Specify window transform - llx:lly:urx:ury"},
	{"viewport", 1, NULL, "Specify viewport transform - llx:lly:urx:ury"},
	{"foreground", 1, NULL, "Default foreground color (X11 color only)"},
	{"background", 1, NULL, "Default background color (X11 color only)"},
	{"reverse", 0, NULL, "Do reverse video (X11 monochrome only)"},
	{"wid", 1, "0", "Drawing window id, 'arg0'"},
	{"ignorebg", 0, NULL, "Ignore CGM requests to change background color"},
	{"pcmap", 0, NULL, "Create a private colormap"},
	{"scmap", 0, NULL, "Use a shared colormap"},
	{"colerr", 1, "10", "Percent Color Error allowed"},
	{"visual", 1, "0", "Visual Id"},
	{NULL}
	};
#endif

#ifdef	CTXT
static	OptDescRec	CTXT_opts[] = {
	{"Data", 0, NULL, "Do suppress display of data"},
	{"Para", 0, NULL, "Do suppress display of parameters"},
	{NULL}
	};
#endif

#if 	defined(SunV) || defined(SunR)
static	OptDescRec	SunV_opts[] = {
	{"Ws", 1, "-1 -1", "Window size"},
	{"Wp", 1, "0 0", "Window position"},
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
        ,{"a60",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"hppcl",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"xwd",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"nrif",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"hdf",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"sgi",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"rgb",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"sun",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"avs",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"parallax",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}
        ,{"yuv",RAST_I,FALSE,TRUE,"", TRUE, raster_opts}

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
