/*
 *	$Id: translate.c,v 1.10 2008-07-27 03:18:44 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include	<stdio.h>
#include	"translate.h"
#include	"default.h"

/*	translate.c:
 *
 *		Author		John Clyne
 *				10/18/88
 *
 *	This file converts VDC coordinates into device coordinates. It 
 *	was modified from the code in ftrans.
 *
 *
 * rev 1.1 clyne 3/7/90		: complete rewrite of transinit routine to 
 *				  accomodate for a device_window specification
 */


#define	ABS(N)		((N) > 0 ? (N) : (-(N)))


long	X_Off,
	Y_Off;
long	R_X_Off,
	R_Y_Off;

double	X_Scale,
	Y_Scale;
double	R_X_Scale,
	R_Y_Scale;

short	devWinSet = FALSE;	/* true if dev_window is not full viewport */


/*
 * default device window coordinates
 */
static	CoordRect	deviceWinCoord = {
				DEVWIN_LLX, DEVWIN_LLY, DEVWIN_URX, DEVWIN_URY
				};

/*
 * we keep track of the device information so SetDevWin() can
 * use it
 */
static	CoordRect	deviceDevExtent;
static	CoordModifier	deviceDevCoordModifier;

static	CoordRect	rasterDevExtent;
static	CoordModifier	rasterDevCoordModifier;

static	short	devIsInit = 0;	/* true after transinit called once for device*/
static	short	rasIsInit = 0;	/* true after transinit called once for raster*/

/*
 * keep track of setting of device window coordinates
 */
static	short	setDevWinCalled;

/*
 * the default for the extent of the device window coordinate system. This
 * is fixed at compile time
 */
static	CoordRect	devWinExtent = {
			DEVWIN_LLX, DEVWIN_LLY, DEVWIN_URX, DEVWIN_URY
			};

static	CoordRect	devViewport = {
			0, 0, 32767, 32767
			};

static	void	coord_map(from, to, map)
	CoordRect	from,
			to;
	CoordModifier	*map;
{
	map->x_scale = (double) (to.urx-to.llx) / (double) (from.urx-from.llx);
	map->y_scale = (double) (to.ury-to.lly) / (double) (from.ury-from.lly);

	map->x_off = to.llx - (map->x_scale * from.llx);
	map->y_off = to.lly - (map->y_scale * from.lly);

}

static	void	aspect_correct(from, to, new)
	CoordRect	from,
			to,
			*new;
{
	unsigned long	from_width,
			from_height,
			to_width,
			to_height,
			new_width,
			new_height;

	double		from_ratio,
			to_ratio;


	from_width = ABS(from.urx - from.llx);
	from_height = ABS(from.ury - from.lly);
	to_width = ABS(to.urx - to.llx);
	to_height = ABS(to.ury - to.lly);

	from_ratio = (double) from_height / (double) from_width;
	to_ratio = (double) to_height / (double) to_width;

	if (from_ratio < to_ratio) {	/* correct along y axis	*/
		new_height = from_ratio * (double) to_width;

		/*
		 * change the y coordinates to give correct aspect ratio
		 */
		if (to.ury > to.lly) {
			to.ury -= (int) ((double) (to_height - new_height) / 2);
			to.lly += (int) ((double) (to_height - new_height) / 2);
		}
		else {
			to.ury += (int) ((double) (to_height - new_height) / 2);
			to.lly -= (int) ((double) (to_height - new_height) / 2);
		}
	}
	else {
		/*
		 * change the x coordinates to give correct aspect ratio
		 */
		new_width = (double) to_height / from_ratio;

		if (to.urx > to.llx) {
			to.urx -= (int) ((double) (to_width - new_width) / 2);
			to.llx += (int) ((double) (to_width - new_width) / 2);
		}
		else {
			to.urx += (int) ((double) (to_width - new_width) / 2);
			to.llx -= (int) ((double) (to_width - new_width) / 2);
		}
	}
		
	new->llx = to.llx;
	new->lly = to.lly;
	new->urx = to.urx;
	new->ury = to.ury;
	
}
 

/*
 *	transinit
 *	[exported]
 *
 *	initialize the coordinate translation macros [X,Y]Convert()
 *	and [X,Y}Scale to translate from virtual to device coodinates.
 *
 * on entry
 *	*dev_extent	: specifies the device coordinates extent
 *      dev_coord_modifier      : additional translation and scaling to
 *                                be performed during translation
 *	device		: if False initialize Raster coordinate coordinate
 *			  translation macros
 * on exit:
 *	*dev_extent	: the device extent corrected for aspect ratio
 *	XScale,
 *	YScale		: scaling required for transformation
 *	X_Off,
 *	Y_Off		: translation required 
 *
 */
void	transinit(dev_extent, dev_coord_modifier, device)
	CoordRect	*dev_extent;
	CoordModifier	dev_coord_modifier;
	int		device;
{
	CoordModifier	map;
	CoordRect	vdc_extent;
	double		xs, ys;
	int		xt, yt;

	xs = (double) (dev_extent->urx - dev_extent->llx) / 32767.0;
	ys = (double) (dev_extent->ury - dev_extent->lly) / 32767.0;
	xt = dev_extent->llx;
	yt = dev_extent->lly;

	dev_extent->llx = xs * devViewport.llx + xt;
	dev_extent->lly = ys * devViewport.lly + yt;
	dev_extent->urx = xs * devViewport.urx + xt;
	dev_extent->ury = ys * devViewport.ury + yt;

	/*
	 * get the vdc extent as defined by the metafile
	 */
	vdc_extent.llx = XMIN;
	vdc_extent.lly = YMIN;
	vdc_extent.urx = XMAX;
	vdc_extent.ury = YMAX;

	/*
	 * record device information so we can export SetDevWin() without
	 * the user having to know about the device
	 */
        if (device) {
                deviceDevExtent = *dev_extent;
		deviceDevCoordModifier = dev_coord_modifier;
		devIsInit = 1;
	}
        else {
                rasterDevExtent = *dev_extent;
		rasterDevCoordModifier = dev_coord_modifier;
		rasIsInit = 1;
	}


	/*
	 * find translation from the device window to the VDC
	 */
	coord_map(devWinExtent, vdc_extent, &map);

	/*
	 * modify the VDC extent as specified by the device window
	 */
	vdc_extent.llx = (deviceWinCoord.llx * map.x_scale) + map.x_off;
	vdc_extent.lly = (deviceWinCoord.lly * map.y_scale) + map.y_off;
	vdc_extent.urx = (deviceWinCoord.urx * map.x_scale) + map.x_off;
	vdc_extent.ury = (deviceWinCoord.ury * map.y_scale) + map.y_off;
	

	/*
	 * modify the device extent to maintain an aspect ratio consistent
	 * with the VDC extent. We change the device extent so that the
	 * largest rectangle the same aspect ratio as defined by the VDC will
	 * fit into the device extent
	 */
	aspect_correct(vdc_extent, *dev_extent, dev_extent);
	

	/*
	 * calculate the mapping from our possibly new vdc extent to our 
	 * possibly new device_extent
	 */
	coord_map(vdc_extent, *dev_extent, &map);

	/*
	 * add in any additional scaling or translation
	 */
	map.x_scale *= dev_coord_modifier.x_scale;
	map.y_scale *= dev_coord_modifier.y_scale;
	map.x_off += dev_coord_modifier.x_off;
	map.y_off += dev_coord_modifier.y_off;

	/*
	 * are we creating translation macros for a device or something 
	 * else
	 */
	if (device) {
		X_Scale = map.x_scale;
		Y_Scale = map.y_scale;
		X_Off = map.x_off;
		Y_Off = map.y_off;

	}
	else {
		R_X_Scale = map.x_scale;
		R_Y_Scale = map.y_scale;
		R_X_Off = map.x_off;
		R_Y_Off = map.y_off;

	}

}

/*
 *	SetDevWin
 *	[exported]
 *
 *	Set the "Device Window" for future rendering as described in the 
 *	"GRAPHCAP Files"
 *	section of the NCAR Graphics Installers Guide. The device window
 *	as defined by llx, lly, urx, and ury is mapped over the entire
 *	viewport window. A normalized window coresponds to the corner
 *	points (0,0) and (32767,32767)
 */
void	SetDevWin(llx, lly, urx, ury)
	long	llx, lly, urx, ury;
{
	CoordRect	dev_extent;

	deviceWinCoord.llx = llx;
	deviceWinCoord.lly = lly;
	deviceWinCoord.urx = urx;
	deviceWinCoord.ury = ury;



	if (devIsInit) {
		dev_extent = deviceDevExtent;
		transinit(&dev_extent, deviceDevCoordModifier, TRUE);
	}

	if (rasIsInit) {
		dev_extent = rasterDevExtent;
		transinit(&dev_extent, rasterDevCoordModifier, FALSE);
	}
        /*
         * if dev_window not set to default record this fact
         */
	devWinSet = ((deviceWinCoord.llx != DEVWIN_LLX) || 
		(deviceWinCoord.lly != DEVWIN_LLY) ||
		(deviceWinCoord.urx != DEVWIN_URX) ||
		(deviceWinCoord.ury != DEVWIN_URY));

	setDevWinCalled = TRUE;
}

/*
 *	GetDevWin
 *	[exported]
 *
 *	Return the current settings for the "Device Window" specification
 */
void	GetDevWin(dev_win_coord)
	CoordRect	*dev_win_coord;
{
	*dev_win_coord = deviceWinCoord;
}

/*
 *	DevWinChanged
 *	[exported]
 *
 *	A query function that returns true if SetDevWin() has been called
 *	since the last time ChangeDevWin() was called.
 */
int	DevWinChanged()
{
	int	set = setDevWinCalled;

	setDevWinCalled = FALSE;
	return (set);
}

void	SetDevViewport(llx, lly, urx, ury)
	long	llx, lly, urx, ury;
{
	devViewport.llx = llx;
	devViewport.lly = lly;
	devViewport.urx = urx;
	devViewport.ury = ury;
}

/*
**
**	convenience routines
**
*/

CoordRect	PackCoordRect(llx, lly, urx, ury)
	long	llx;
	long	lly;
	long	urx;
	long	ury;
{
	CoordRect	cr;

	cr.llx = llx;
	cr.lly = lly;
	cr.urx = urx;
	cr.ury = ury;
	return(cr);
}

