/*
 *	$Id: rast.c,v 1.15 1992-07-30 00:47:47 clyne Exp $
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
#include <errno.h>
#include <ncarv.h>
#include <ncarg_ras.h>
#include "cgmc.h"
#include "rast.h"
#include "translate.h"
#include "ctrandef.h"
#include "default.h"
#include "devices.h"

extern	boolean	*softFill;
extern	boolean	deviceIsInit;
extern  int  	currdev;
extern  int  	optionDesc;

extern	char	*RasterGetError();
extern	char	*strcpy();


static	struct	Opts {
	char	*resolution;
	char	*window;
	char	*viewport;
	char	*dpi;
	boolean	rle;
	boolean	compress;
	boolean	direct;
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
	{"direct",NCARGCvtToBoolean,(Voidptr) &rast_opts.direct,					sizeof (rast_opts.direct)
	},
	{
	NULL
	}
};

RasColrTab	colorTab;	/* the color table			*/

Raster	*rastGrid;		/* struct for creating output file	*/
boolean	rasIsDirect;		/* direct encoded image?		*/
static	CoordRect	VDCExtent;

static	boolean	ras_is_clipped(offset)
	long	offset;
{
	extern	long	clipxmin, clipxmax, clipymin, clipymax;
	int	x,y;
	int	xmin = XConvert(clipxmin);
	int	xmax = XConvert(clipxmax);
	int	ymin = YConvert(clipymax); /* CGM and rast origin switched */
	int	ymax = YConvert(clipymin); /* CGM and rast origin switched */

	if (offset < 0) return (TRUE);

	if (rasIsDirect) {
		offset /= 3;
	}
	y = offset / rastGrid->nx;
	x = offset % rastGrid->nx;

	if (x < xmin || x > xmax) return (TRUE);
	if (y < ymin || y > ymax) return (TRUE);
	return(FALSE);
}
	

/*
 * 	Class 0 Function
 */
/*ARGSUSED*/
int	Ras_BegMF(c)
CGMC *c;
{

	CoordRect	dev_extent;
	CoordModifier	coord_mod;
	int	width, height;
	int	encoding;

	int	ras_argc;
	char	*ras_argv[10];

	Raster	*RasterOpenWrite();
	int	status = 0;

	/*
	 *      parse raster specific command line args
	 *      (currently only resolution accepted       )
	 */
	if (GetOptions(optionDesc, raster_opts) < 0) {
		ESprintf(
			E_UNKNOWN,"GetOptions(%d,) [ %s ]",
			optionDesc, ErrGetMsg()
		);
		return(-1);

	}
	rasIsDirect = rast_opts.direct;

	/*
	 * load options for libraster into ras_argv and initialilze
	 * libraster. We don't have direct access to argc and argv so
	 * we use this kludge.
	 */
	build_ras_arg(&ras_argc, ras_argv, rast_opts);
	if (RasterInit(&ras_argc, ras_argv) != RAS_OK) {
		ESprintf(E_UNKNOWN, "RasterInit(,)");
		return(-1);
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
			ESprintf(
				E_UNKNOWN,
				"Invalid viewport format [ %s ]", ErrGetMsg()
			);
			status = -1;
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
			ESprintf(
				E_UNKNOWN,
				"Invalid window format [ %s ]", ErrGetMsg()
			);
			status = -1;
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
	encoding = rasIsDirect ? RAS_DIRECT : RAS_INDEXED;
	if ((rastGrid = (RasterOpenWrite("stdout", width, height,
		VERSION,encoding,devices[currdev].name))) == NULL) {

		ESprintf(E_UNKNOWN, "RasterOpenWrite(,,,,,)");
		return(-1);
	}

	/*
	 * clear the grid
	 */
	clear_grid(rastGrid);

	deviceIsInit = TRUE;
	return (status);
}

/*ARGSUSED*/
int	Ras_EndMF(c)
CGMC *c;
{

	if (!deviceIsInit) {
		return(0);
	}

	RasterClose(rastGrid);

	deviceIsInit = FALSE;
	return (0);
}


/*ARGSUSED*/
int	Ras_BegPic(c)
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
	return (0);
}

/*ARGSUSED*/
int	Ras_BegPicBody(c)
CGMC *c;
{
        VDCExtent.llx = XMIN;
        VDCExtent.lly = YMIN;
        VDCExtent.urx = XMAX;
        VDCExtent.ury = YMAX;


	if (BACKCOLR_DAMAGE) {
		set_back_color(BACKCOLR);
		BACKCOLR_DAMAGE = FALSE;
	}

	return (0);
}

/*ARGSUSED*/
int	Ras_EndPic(c)
CGMC *c;
{

	int	i;

	/*
	 * copy the color map into the Raster*
	 */
	if (! rasIsDirect) {
		for(i=0; i<256; i++) {
			INDEXED_RED(rastGrid, i) = colorTab.rgb[i].red;
			INDEXED_GREEN(rastGrid, i) = colorTab.rgb[i].green;
			INDEXED_BLUE(rastGrid, i) = colorTab.rgb[i].blue;
		}
	}

	/*
	 *	 write the file
	 */
	 if( RasterWrite(rastGrid) != RAS_OK) {
		ESprintf(E_UNKNOWN, "RasterWrite()");
		return (-1);
	}

	

	/*
	 * reset to default attributes
	 */
	(void)SetInPic((boolean)FALSE);
	return (0);
}

/*ARGSUSED*/
int	Ras_ClearDevice(c)
CGMC *c;
{
	clear_grid(rastGrid);
	return(0);
}

int	Ras_PolyMarker(c)
CGMC *c;
{
	int	_PolyMarker();

	return(_PolyMarker(c, FALSE));
}


/*ARGSUSED*/
int	Ras_CellArray(c)
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

	int	ras_cell_array(), ras_non_rect_cell_array();

	if (COLOUR_TABLE_DAMAGE) {
		rast_update_color_table();
		COLOUR_TABLE_DAMAGE = FALSE;
	}
	if (CLIP_DAMAGE) {
		CoordRect       device_win_coord;
		GetDevWin(&device_win_coord);
		gcap_set_clip(device_win_coord, VDCExtent, CLIPFLAG);
		CLIP_DAMAGE = FALSE;
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

	if (CSM != MODE_INDEXED) {
		ESprintf(EINVAL, "direct color not supported");
		return (-1);
	}

	if (mode != PACKED_MODE) {
		ESprintf(EINVAL, "run length encoding not supported");
		return(-1);
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



static	clear_grid(grid)
	Raster	*grid;
{
	if (rasIsDirect) {
		int	x, y;

		for(y=0; y<grid->ny; y++)
		for(x=0; x<grid->nx; x++) {
			DIRECT_RED(grid, x, y) = colorTab.rgb[0].red;
			DIRECT_GREEN(grid, x, y) = colorTab.rgb[0].green;
			DIRECT_BLUE(grid, x, y) = colorTab.rgb[0].blue;
		}
	}
	else {
		bzero((char *) grid->data, grid->nx * grid->ny);
	}
}

static	init_color_tab()
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
	
static	set_back_color(colr)
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
			/*
			 * error
			 */
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
static	int	ras_non_rect_cell_array(c, Pcoord, Qcoord, Rcoord, nx, ny)
	CGMC		*c;
        Ptype  Pcoord, Qcoord, Rcoord;
        int     nx, ny;
{
        return(0);      /* non rectangular cell arrays are not supported */
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
static	int	ras_cell_array(c, Pcoord, Qcoord, Rcoord, nx, ny)
	CGMC		*c;
	Ptype	Pcoord, Qcoord, Rcoord;
	int	nx, ny;
{
	unsigned int	image_height,	/* image height in pixels	*/
			image_width,	/* image width in pixels	*/
			image_size,	/* size of image data in bytes	*/
			pad,
			pixel_size,	/* pixel size in bytes		*/
			bytes_per_line;	/* bytes per scan line		*/
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
	if (nx == 0 || ny == 0) return (0);

	rows = (int *) icMalloc ((unsigned) ny * sizeof (int));
	cols = (int *) icMalloc ((unsigned) nx * sizeof (int));
	index_array = (unsigned char *) icMalloc ((unsigned) nx * sizeof (int));

	pixel_size = rasIsDirect ? 3 : 1;
	bytes_per_line = rastGrid->nx * pixel_size;

	image_size = image_height * bytes_per_line;
	pad = pixel_size * (rastGrid->nx - image_width);
	data = rastGrid->data;

 
	/*
	 * calculate x & y steping size, position of image in the window,
	 * and starting address for data destination
	 */
	xoff = MIN3(Pcoord.x, Qcoord.x, Rcoord.x) * pixel_size;
	yoff = MIN3(Pcoord.y, Qcoord.y, Rcoord.y);
	SetUpCellArrayAddressing(
		Pcoord, Qcoord, Rcoord, image_size, pad, pixel_size,
		bytes_per_line, xoff, yoff, &step_x, &step_y, 
		&start_x, &start_y, (char **) &data
	);

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
					return (-1);
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

				if (!ras_is_clipped(
					(long) (cptr - rastGrid->data))
					) {

				if (rasIsDirect) {
					cptr[0] = colorTab.rgb[index].red;
					cptr[1] = colorTab.rgb[index].green;
					cptr[2] = colorTab.rgb[index].blue;
				}
				else {
					*cptr = index;
				}
				}	/* clipped	*/

				cptr += step_x;
				}	/* for	*/
			
			}
			data += step_y;	/* skip to next row	*/
		}
	}

	free((char *) rows);
	free((char *) cols);
	free((char *) index_array);

	return(0);
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
