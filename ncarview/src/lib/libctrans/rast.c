/*
 *	$Id: rast.c,v 1.2 1991-06-18 15:01:50 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
		
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <ncarv.h>
#include <ncarg_ras.h>
#include <cterror.h>
#include "cgmc.h"
#include "rast.h"
#include "translate.h"
#include "ctrandef.h"
#include "default.h"
#include "devices.h"

extern	boolean	*softFill;
extern	boolean	deviceIsInit;
extern  int  	currdev;


static	struct	RasterCommLineOpt_ {
	StringType_	resolution;
	StringType_	dpi;
	BoolType_	rle;
	BoolType_	compress;
	} rasterCommLineOpt;

static	Option	raster_opts[] = {
	{
	"resolution",StringType,(unsigned long) &rasterCommLineOpt.resolution,
				sizeof (StringType_)
	},
	{
	"dpi",StringType,(unsigned long) &rasterCommLineOpt.dpi,
				sizeof(StringType_)
	},
	{
	"rle",BoolType,(unsigned long) &rasterCommLineOpt.rle,sizeof(BoolType_)
	},
	{
	"compress",BoolType,(unsigned long) &rasterCommLineOpt.compress,					sizeof (BoolType_)
	},
	{
	NULL
	}
};

RasColrTab	colorTab;	/* the color table		*/

Raster	*rastGrid;		/* struct for creating output file	*/

/*
 * 	Class 0 Function
 */
/*ARGSUSED*/
Ct_err	Ras_BegMF(c)
CGMC *c;
{

	CoordRect	dev_extent;
	CoordModifier	coord_mod;
	int	width, height;

	int	ras_argc;
	char	*ras_argv[10];

	Raster	*RasterOpenWrite();

	/*
	 *      parse raster specific command line args
	 *      (currently only resolution accepted       )
	 */
	getOptions((caddr_t) 0, raster_opts);

	/*
	 * load options for libraster into ras_argv and initialilze
	 * libraster. We don't have direct access to argc and argv so
	 * we use this kludge.
	 */
	build_ras_arg(&ras_argc, ras_argv, rasterCommLineOpt);
	if (RasterInit(&ras_argc, ras_argv) != RAS_OK) {
		ct_error(T_NULL, RasterGetError());
		return(DIE);
	}

	/*
	 * find dimensions of buffer for rasterization
	 */
	get_resolution(&dev_extent, rasterCommLineOpt, devices[currdev].name);
	width = ABS(dev_extent.llx - dev_extent.urx) + 1;
	height = ABS(dev_extent.lly - dev_extent.ury) + 1;

	
	/*
	 * 	Init translation values and the formating routines
	 */
	coord_mod.x_off = 0;
	coord_mod.y_off = 0;
	coord_mod.x_scale = 1.0;
	coord_mod.y_scale = 1.0;

	transinit(&dev_extent, coord_mod, TRUE);

	/*
	 * initialize the software fill module. This needs to
	 * be initialized every time the window changes sizes
	 */
	initSoftSim(height, width);

	/*
	 * tweek soft fill option to do software filling 
	 */
	*softFill = TRUE;

	init_color_tab();


	/*
	 * create the raster buffer
	 */
	if ((rastGrid = (RasterOpenWrite("stdout", width, height,
		"NCAR View 3.01",RAS_INDEXED,devices[currdev].name))) == NULL) {

		ct_error(T_NULL, RasterGetError());
		return (DIE);
	}

	deviceIsInit = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err	Ras_EndMF(c)
CGMC *c;
{

	if (!deviceIsInit) {
		return(OK);
	}

	RasterClose(rastGrid);

	deviceIsInit = FALSE;
	return (OK);
}


/*ARGSUSED*/
Ct_err	Ras_BegPic(c)
CGMC *c;
{
	int	i;
	static	boolean	firstFrame = TRUE;

	/*
	 *      copy default table to working default table
	 *      most of the CGM elements contain output attribute or
	 *      input processing information. This data is stored in a
	 *      table in "default.h". SetInPic keeps the data up to date
	 *      for each new frame
	 */
	SetInPic((boolean)TRUE);

	if (firstFrame) {
		/*
		 * from now on can't modify default color map, can only
		 * modify current colormap
		 */
		colorTab.rgb = &colorTab.current_rgb[0];
		firstFrame = FALSE;
	}

	/*
	 * restore default global colormap to current colormap
	 */
	for (i=0; i < MAX_COLOR; i++) {
		colorTab.current_rgb[i] = colorTab.default_rgb[i];
	}

	FILL_COLOUR_DAMAGE = TRUE;
	LINE_COLOUR_DAMAGE = TRUE;
	LINE_WIDTH_DAMAGE = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err	Ras_BegPicBody(c)
CGMC *c;
{
	if (BACKCOLR_DAMAGE) {
		set_back_color(BACKCOLR);
		BACKCOLR_DAMAGE = FALSE;
	}

	clear_grid(rastGrid);
	return (OK);
}

/*ARGSUSED*/
Ct_err	Ras_EndPic(c)
CGMC *c;
{

	int	i;

	/*
	 * copy the color map into the Raster*
	 */
	for(i=0; i<256; i++) {
		INDEXED_RED(rastGrid, i) = colorTab.rgb[i].red;
		INDEXED_GREEN(rastGrid, i) = colorTab.rgb[i].green;
		INDEXED_BLUE(rastGrid, i) = colorTab.rgb[i].blue;
	}

	/*
	 *	 write the file
	 */
	 if( RasterWrite(rastGrid) != RAS_OK) {
		ct_error(NT_NULL, RasterGetError());
		return (DIE);
	}

	

	/*
	 * reset to default attributes
	 */
	(void)SetInPic((boolean)FALSE);
	return (OK);
}


/*ARGSUSED*/
Ct_err	Ras_CellArray(c)
CGMC *c;
{
#define	PACKED_MODE	1

	/* points giving boundry of cell array	*/
	Ptype	Pcoord,		/* LOWER left corner 			*/
		Qcoord,		/* upper right corner			*/
		Rcoord;		/* lower right				*/	

	Itype	nx, ny;		/* dimensions of cell array by number of cells*/
	Etype	mode;		/* cell representation mode		*/

	Ct_err	cell_array();


	/*
 	 *	extract data from cgmc
	 */

		/*	corners		*/
	Pcoord.x = XConvert(c->p[0].x);	Pcoord.y = YConvert(c->p[0].y);
	Qcoord.x = XConvert(c->p[1].x);	Qcoord.y = YConvert(c->p[1].y);
	Rcoord.x = XConvert(c->p[2].x);	Rcoord.y = YConvert(c->p[2].y);

		/*	dimensions	*/
	nx = c->i[0];		ny = c->i[1];


		/*	cell representation mode	*/
	mode = c->e[0];

	if (CSM != INDEXED) {
		ct_error(NT_CAFE, "direct color not supported");
		return (SICK);
	}

	if (mode != PACKED_MODE) {
		ct_error(NT_CAFE, "packed mode only supported");
		return (SICK);
	}

	/*
	 * only support rectangular cell arrays
	 */
	if (Pcoord.y == Rcoord.y && Rcoord.x == Qcoord.x) {
		int	*rows, *cols;

		cols = (int *) icMalloc((unsigned) nx * sizeof (int));
		rows = (int *) icMalloc((unsigned) ny * sizeof (int));

		cell_prep(Pcoord, Qcoord, Rcoord, rows, cols, 
					(unsigned) nx, (unsigned) ny);
		
		(void) cell_array(c, Pcoord, rows, cols, 
			(int) nx, (int) ny, abs((int) (Pcoord.x - Rcoord.x)));

		if (rows) cfree((char *) rows);
		if (cols) cfree((char *) cols);
	}

	/* 
	 * cell array is NOT rectangular
	 */
	else {
	
		ct_error(NT_CAFE, "cell array must be rectangular");
		return (SICK);
	}
	return (OK);
}

Ct_err	Ras_ColrTable(c)
CGMC *c;
{
	int	i,
		index;

	/*
	 * make sure color table does not exceed device capabilities
	 */
	if (c->ci[0] >= MAX_COLOR || c->ci[0] < 0) return(OK);

	for (index=c->ci[0], i = 0 ;index< (c->ci[0] + c->CDnum); index++,i++) {
		
		colorTab.rgb[index].red =  c->cd[i].red;
		colorTab.rgb[index].green =  c->cd[i].green;
		colorTab.rgb[index].blue =  c->cd[i].blue;
	}

	return (OK);
}

clear_grid(grid)
	Raster	*grid;
{
	int	x,y;

#ifdef	DEAD
	for(y = 0; y < grid->ny; y++)
	for(x=0; x < grid->nx; x++) {
		INDEXED_PIXEL(grid, x, y) = 0;
	}
#else
	bzero((char *) grid->data, grid->nx * grid->ny);
#endif
	
}

init_color_tab()
{
	int	i;


	/*
	 * set rgb-component pointers to default portion of 
	 * RasColorMap
	 */
	colorTab.rgb = &colorTab.default_rgb[0];

	/*
	 * record the foreground and background color in the
	 * colorTab. These values can be overwritten by the CGM
	 */
	colorTab.rgb[0].red = colorTab.rgb[0].green = colorTab.rgb[0].blue = 0;

	colorTab.rgb[1].red = colorTab.rgb[1].green = colorTab.rgb[1].blue = 255;

}
	
set_back_color(colr)
	CDtype	colr;
{
	colorTab.rgb[0].red = colr.red;
	colorTab.rgb[0].green = colr.green;
	colorTab.rgb[0].blue = colr.blue;
}

#define DEFAULT_WIDTH   512	/* default raster width         */
#define DEFAULT_HEIGHT  512	/* default raster height        */
get_resolution(dev_extent, opts, name)
	CoordRect	*dev_extent;
	struct	RasterCommLineOpt_ opts;
	char	*name;
{
	int	width = DEFAULT_WIDTH;
	int	height = DEFAULT_HEIGHT;
	int	dpi;

	/*
	 * this is a hack to figure out the resolution for output to an
	 * hp laserjet. With the laserjet users specify resolution in terms
	 * of dots per inch.  We assume a full size picture is 6 by 6 inches
	 */
	if (! strcmp(name, "hplj")) {
		dpi = atoi(opts.resolution);
		width = 6 * dpi;
		height = 6 * dpi;
	}
	else {
		char	*cptr = opts.resolution;

		if (sscanf(cptr, "%dx%d", &width, &height) != 2){
			ct_error(NT_NULL, 
			"Error parsing resolution, using defaults");
		}
	}

	/*
	 * 	Init translation values and the formating routines
	 */
	dev_extent->llx = 0;
	dev_extent->lly = height - 1;
	dev_extent->ury = 0;
	dev_extent->urx = width - 1;
}

Ct_err	cell_array(c, P, rows, cols, nx,  ny, width)
	CGMC	*c;
	Ptype	P;
	int	*rows, *cols;
	int	nx, ny;
	int	width;
{

	register	int k,l;
	register	int i,j;

	int	*index_array = NULL;	/* single row of cell pixel vals*/
	int	index = 0;      	/* index for color list in cgmc */
	int	x_index;		/* x coordinate			*/

	index_array = (int *) icMalloc ((unsigned) nx * sizeof(int));


	/*	the rows	*/
	for (i=0; i < ny; i++ ) {

		/* load  array of color indecies for that row of cells	*/
		for (k=0; k < nx; k++, index++) {

			/* make sure data available in cgmc     */
			if (index == c->Cnum && c->more) {
				if (Instr_Dec(c) != OK)
					return (pre_err);

				index = 0;
			}

			index_array[k] = c->c[index];
		}
		
		/*	the rows of pixels per cell	*/
		for (j=0; j < rows[i]; j++, P.y--) {


			/*	the coloumns	*/
			x_index = P.x;
			for (k=0; k<nx; k++) {


				/*	the coloums of pixels per cell	*/
				for (l=0; l< cols[k]; l++, x_index++ ) {
					INDEXED_PIXEL(rastGrid, x_index, P.y) =
								index_array[k];
	
				}
			}
		}

	
	}

	if (index_array != (int *) NULL) cfree((char *) index_array);

	return (OK);
}

build_ras_arg(ras_argc, ras_argv, rasterCommLineOpt)
	int	*ras_argc;
	char	**ras_argv;
	struct	RasterCommLineOpt_ rasterCommLineOpt;
{
	int	i;

	i = 0;

	ras_argv[i] = icMalloc(strlen("ctrans") + 1);
	strcpy(ras_argv[i], "ctrans");
	i++;

	ras_argv[i] = icMalloc(strlen("-dpi") + 1);
	strcpy(ras_argv[i], "-dpi");
	i++;

	ras_argv[i] = icMalloc(strlen(rasterCommLineOpt.dpi) + 1);
	strcpy(ras_argv[i], rasterCommLineOpt.dpi);
	i++;

	if (rasterCommLineOpt.compress) {
		ras_argv[i] = icMalloc(strlen("-compress") + 1);
		strcpy(ras_argv[i], "-compress");
		i++;
	}

	if (rasterCommLineOpt.rle) {
		ras_argv[i] = icMalloc(strlen("-rle") + 1);
		strcpy(ras_argv[i], "-rle");
		i++;
	}

	*ras_argc = i;
}
