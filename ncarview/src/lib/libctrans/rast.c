/*
 *	$Id: rast.c,v 1.10 1992-04-03 20:58:01 clyne Exp $
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

extern	char	*RasterGetError();
extern	char	*strcpy();


static	struct	Opts {
	char	*resolution;
	char	*window;
	char	*viewport;
	char	*dpi;
	boolean	rle;
	boolean	compress;
	} rast_opts;

static	Option	raster_opts[] = {
	{"resolution",NCARGCvtToString,(Voidptr) &rast_opts.resolution,
				sizeof (rast_opts.resolution)
	},
	{"window",NCARGCvtToString,(Voidptr) &rast_opts.window,
				sizeof (rast_opts.window)
	},
	{"viewport",NCARGCvtToString,(Voidptr) &rast_opts.viewport,
				sizeof (rast_opts.viewport)
	},
	{"dpi",NCARGCvtToString,(Voidptr) &rast_opts.dpi,
				sizeof(rast_opts.dpi)
	},
	{"rle",NCARGCvtToBoolean,(Voidptr) &rast_opts.rle,sizeof(rast_opts.rle)
	},
	{"compress",NCARGCvtToBoolean,(Voidptr) &rast_opts.compress,					sizeof (rast_opts.compress)
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
	if (GetOptions(raster_opts) < 0) {
		ct_error(T_NULL, ErrGetMsg());
		return(DIE);
	}

	/*
	 * load options for libraster into ras_argv and initialilze
	 * libraster. We don't have direct access to argc and argv so
	 * we use this kludge.
	 */
	build_ras_arg(&ras_argc, ras_argv, rast_opts);
	if (RasterInit(&ras_argc, ras_argv) != RAS_OK) {
		ct_error(T_NULL, RasterGetError());
		return(DIE);
	}

	/*
	 * find dimensions of buffer for rasterization
	 */
	get_resolution(&dev_extent, rast_opts, devices[currdev].name);
	width = ABS(dev_extent.llx - dev_extent.urx) + 1;
	height = ABS(dev_extent.lly - dev_extent.ury) + 1;

	
	/*
	 * 	Init translation values and the formating routines
	 */
	coord_mod.x_off = 0;
	coord_mod.y_off = 0;
	coord_mod.x_scale = 1.0;
	coord_mod.y_scale = 1.0;

	/*
	 * set device viewport specification
	 */
	if (rast_opts.viewport) {
		int	llx, lly, urx, ury;

		if (CoordStringToInt(rast_opts.viewport,&llx,&lly,&urx,&ury)<0){
			ct_error(NT_NULL, rast_opts.window);
		}
		else {
			SetDevViewport(
				(long) llx, (long) lly,(long) urx,(long) ury
			);
		}
	}

	/*
	 * set device window specification
	 */
	if (rast_opts.window) {
		int	llx, lly, urx, ury;

		if (CoordStringToInt(rast_opts.window,&llx,&lly,&urx,&ury)<0){
			ct_error(NT_NULL, rast_opts.window);
		}
		else {
			SetDevWin((long) llx, (long) lly,(long) urx,(long) ury);
		}
	}

	transinit(&dev_extent, coord_mod, TRUE);

	/*
	 * initialize the software fill module. This needs to
	 * be initialized every time the window changes sizes
	 */
	initSoftSim(0, width-1, 0, height-1);

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

	/*
	 * clear the grid
	 */
	clear_grid(rastGrid);

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
Ct_err	Ras_ClearDevice(c)
CGMC *c;
{
	clear_grid(rastGrid);
	return(OK);
}

Ct_err	Ras_PolyMarker(c)
CGMC *c;
{
	Ct_err	_PolyMarker();

	return(_PolyMarker(c, FALSE));
}


/*ARGSUSED*/
Ct_err	Ras_CellArray(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"Ras_CellArray\n");
#endif DEBUG

#define	PACKED_MODE	1

	void	rast_update_color_table();

	/*	
	 *	programmers unfamiliar with CGM representation of Cell arrays
	 *	should see section 5.6.9  in the ANSI document on 
	 *	Computer Graphic Metafiles.
	 */


	/* points giving boundry of cell array	*/
	Ptype	Pcoord, 
		Qcoord, 
		Rcoord;	/* cell array corner boundries		*/
	int	nx, ny;		/* dimensions of cell array by number of cells*/
	Etype	mode;		/* cell representation mode		*/

	Ct_err	ras_cell_array(), ras_non_rect_cell_array();

	if (COLOUR_TABLE_DAMAGE) {
		rast_update_color_table();
		COLOUR_TABLE_DAMAGE = FALSE;
	}

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
		(void) fprintf(stderr, 
		"ctrans: run length encoded cell arrays not supported\n");
		return(OK);
	}

        /*
         * see if cell array is rectangular or not
         */
        if (Rcoord.x != Qcoord.x || Pcoord.y != Rcoord.y) {
                return (ras_non_rect_cell_array(c,Pcoord,Qcoord,Rcoord,nx,ny));
        }

        /*
         * cell array is a rectangluar
         */
        return(ras_cell_array(c, Pcoord, Qcoord, Rcoord, nx, ny));

}



clear_grid(grid)
	Raster	*grid;
{
#ifdef	DEAD
	int	x,y;

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

	/*
	 * set rgb-component pointers to default portion of 
	 * RasColorMap
	 */
	colorTab.rgb = &colorTab.default_rgb[0];

	/*
	 * record the foreground and background color in the
	 * colorTab. These values can be overwritten by the CGM
	 */
	colorTab.rgb[0].red = colorTab.rgb[0].green = colorTab.rgb[0].blue =0;
	colorTab.rgb[1].red = colorTab.rgb[1].green = colorTab.rgb[1].blue =255;

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
	struct	Opts opts;
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



/*ARGSUSED*/
static	Ct_err	ras_non_rect_cell_array(c, Pcoord, Qcoord, Rcoord, nx, ny)
	CGMC		*c;
        Ptype  Pcoord, Qcoord, Rcoord;
        int     nx, ny;
{
        return(OK);      /* non rectangular cell arrays are not supported */
}



/*
 *	ras_cell_array
 *	[internal]
 *
 *	render a rectangular cell array
 *
 * on entry
 *	P,Q,R		: corners of the cell array (See CGM standard)
 *	nx		: number of cells in x direction
 *	ny		: number of cells in y direction
 * on exit
 *	return		: 0 => Ok, else error
 */
static	Ct_err	ras_cell_array(c, Pcoord, Qcoord, Rcoord, nx, ny)
	CGMC		*c;
	Ptype	Pcoord, Qcoord, Rcoord;
	int	nx, ny;
{
	unsigned int	image_height,	/* image height in pixels	*/
			image_width,	/* image width in pixels	*/
			image_size,	/* size of image data in bytes	*/
			pad;
	unsigned char	*data,		/* image data			*/
			*cptr;

	int		step_x,		/* step size for incrementing in
					 * x direction within the image
					 */
			step_y;		/* step size for incrementing in
					 * y direction within the image
					 */

	int		start_x, 
			start_y;	/* destination of image in drawable */

	int		*rows, 
			*cols;		/* information about the number of
					 * pixels making up a row (col) in
					 * a the cell at row (col)[i]
					 */
	int		xoff, yoff;	/* offset to cell array dest.	*/
	unsigned char	*index_array,	/* color indeces for a cell row	*/
			index;		/* color index for current cell */
	int		cgmc_index;	/* index into the cgmc		*/


	register int	i,j,k,l;

	void	SetUpCellArrayIndexing(), SetUpCellArrayAddressing();

	image_width = ABS(Pcoord.x - Qcoord.x) + 1;
	image_height = ABS(Pcoord.y - Qcoord.y) + 1;

	/*
	 * don't know how to handle a cell array with zero dimension
	 */
	if (nx == 0 || ny == 0) return (OK);

	rows = (int *) icMalloc ((unsigned) ny * sizeof (int));
	cols = (int *) icMalloc ((unsigned) nx * sizeof (int));
	index_array = (unsigned char *) icMalloc ((unsigned) nx * sizeof (int));

	image_size = image_height * rastGrid->nx;
	pad = rastGrid->nx - image_width;
	data = rastGrid->data;

 
	/*
	 * calculate x & y steping size, position of image in the window,
	 * and starting address for data destination
	 */
	xoff = MIN3(Pcoord.x, Qcoord.x, Rcoord.x);
	yoff = MIN3(Pcoord.y, Qcoord.y, Rcoord.y);
	SetUpCellArrayAddressing(Pcoord, Qcoord, Rcoord, image_size, pad, 1,
		(unsigned) rastGrid->nx, xoff, yoff, 
		&step_x, &step_y, &start_x, &start_y, (char **) &data);

	/*
	 * set up rows and cols arrays with info about number of pixels
	 * making up each cell. We do this to avoid floating point arithmatic
	 * later on
	 */
	SetUpCellArrayIndexing(image_width, image_height, 
				rows, cols, (unsigned) nx, (unsigned) ny);


	/*
	 * process the rows
	 */
	cgmc_index = 0;
	for (i=0; i<ny; i++) {

		/* 
		 * load array of color indecies for row[i] of cells
		 */
                for (k=0; k<nx; k++) {

			/* make sure data available in cgmc     */
			if (cgmc_index >= c->Cnum && c->more) {
				if (Instr_Dec(c) < 1) {
					ct_error(T_FRE, "metafile");
					return (DIE);
				}
				cgmc_index = 0;
			}
                        index_array[k] = (unsigned char) c->c[cgmc_index];
			cgmc_index++;
                }

                /*      
		 * the rows of pixels per cell[i]
		 */
                for (j=0; j < rows[i]; j++) {

			cptr = data;
			/*
			 * the coloumns
			 */
			for (k=0; k<nx; k++) {


				/*
				 * the coloums of pixels per cell
				 */
				index = index_array[k];
				for (l=0; l<cols[k]; l++) {
					*cptr = index;
					cptr += step_x;
				}
			
			}
			data += step_y;	/* skip to next row	*/
		}
	}

	free((char *) rows);
	free((char *) cols);
	free((char *) index_array);

	return(OK);
}







static	build_ras_arg(ras_argc, ras_argv, rast_opts)
	int	*ras_argc;
	char	**ras_argv;
	struct	Opts rast_opts;
{
	int	i;

	i = 0;

	ras_argv[i] = icMalloc((unsigned) strlen("ctrans") + 1);
	(void) strcpy(ras_argv[i], "ctrans");
	i++;

	ras_argv[i] = icMalloc((unsigned) strlen("-dpi") + 1);
	(void) strcpy(ras_argv[i], "-dpi");
	i++;

	ras_argv[i] = icMalloc((unsigned) strlen(rast_opts.dpi) + 1);
	(void) strcpy(ras_argv[i], rast_opts.dpi);
	i++;

	if (rast_opts.compress) {
		ras_argv[i] = icMalloc((unsigned) strlen("-compress") + 1);
		(void) strcpy(ras_argv[i], "-compress");
		i++;
	}

	if (rast_opts.rle) {
		ras_argv[i] = icMalloc((unsigned) strlen("-rle") + 1);
		(void) strcpy(ras_argv[i], "-rle");
		i++;
	}

	*ras_argc = i;
}
