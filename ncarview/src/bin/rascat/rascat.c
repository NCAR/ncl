
/*
 *      $Id: rascat.c,v 1.1 1992-02-13 17:49:19 clyne Exp $
 */
/*
 *	File:		rascat.c
 *
 *	Author:		Don Middleton-Link
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Rev 1.1		John Clyne
 *
 *	Date:		Wed Feb 12 20:12:58 MST 1992
 *
 *	Description:	Read raster files and concatenate them to the
 *			standard output. The input files need not be of 
 *			the same format but they must have the same resolution
 *			and encoding.
 *
 *	Syntax:		rascat [-v] [-V] [-if format] [-of format] 
 *			[-win nx ny x y] [-o file] [ - | file... ]
 *
 *	Options:
 *
 *			-v	Operate in verbose mode
 *
 *			-V	Print version number to the tty
 *
 *			-if <format>
 *				'format' specifies the file format of *all*
 *				the input raster files. If ommitted, file
 *				format is determined by the file name extension
 *
 *			-of <format>
 *				'format' specifies the file format of the raster
 *				file to be written to. If ommitted, the output
 *				file format will be the same as the format 
 *				of the *first* input file. 
 *
 *			-win <nx ny x y>
 *				nx, ny, x, and y specify a sub-region of the
 *				input file. 'x' and 'y' specify the upper-left 
 *				corner. 'nx' and 'ny' specify the width 
 *				and height of the region, respectively. The
 *				default is to copy the entire image.
 *
 *			-o <file>
 *				Write output to 'file' instead of the standard
 *				output.
 *
 *			- 	Read from standard input. The default.
 *
 *	Environment:
 *				
 */
#include <stdio.h>
#ifdef	SYSV
#include <strings.h>
#else
#include <string.h>
#endif
#include <ncarg_ras.h>

static	void	Usage(msg) 
	char	*msg;
{
	char	*opts = "[-v] [-if format] [-of format] [-win nx ny x y] [-o file] [ - | file... ]";

	if (msg) {
		fprintf(stderr, "rascat: %s\n", msg);
	}
	fprintf(stderr, "rascat: %s\n", opts);
}

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	do_verbose = False;
	int	do_version = False;
	int	do_window  = False;

	char	*dstfile = "stdout";
	char	*srcformat = (char *) NULL;
	char	*dstformat = (char *) NULL;
	char	*Comment = "Created by rascat";
	char	*arg;
	Raster	*src, *dst, *RasterOpen(), *RasterOpenWrite();
	int	frame = 0;
	int	src_x, src_nx, src_y, src_ny;
	char	**rfiles;
	int	rcount = 0;
	int	status;
	int	nX, nY;
	RasterEncoding	rasterType;
	int	i;

	char	*prog_name;

	prog_name = (prog_name = strrchr(argv[0],'/')) ? ++prog_name : *argv;

	(void) RasterInit(&argc, argv);

	rfiles = (char **) malloc(argc * sizeof(char *));
	if (! rfiles) {
		perror(prog_name);
		exit(1);
	}

	while(--argc) {
		arg = *++argv;

		if (!strcmp(arg, "-")) {
			continue;
		}
		else if (!strcmp(arg, "-v")) {
			do_verbose = True;
		}
		else if (!strcmp(arg, "-V")) {
			do_version = True;
		}
		else if (!strcmp(arg, "-o")) {
			if (argc >= 1) {
				argc--;
				dstfile = *++argv;
			}
			else {
				Usage("ran out of args");
			}
		}
		else if (!strcmp(arg, "-if")) {
			if (argc >= 1) {
				argc--;
				srcformat = *++argv;
			}
			else {
				Usage("ran out of args");
			}
		}
		else if (!strcmp(arg, "-of")) {
			if (argc >= 1) {
				argc--;
				dstformat = *++argv;
			}
			else {
				Usage("ran out of args");
			}
		}
		else if (!strcmp(arg, "-win")) {
			if (argc >= 4) {
				argc -= 4;
				do_window = True;
				src_nx = atoi(*++argv);
				src_ny = atoi(*++argv);
				src_x  = atoi(*++argv);
				src_y  = atoi(*++argv);
			}
			else {
				Usage("ran out of args");
			}
		}
		else if (*arg == '-') {
			Usage("invalid option");
		}
		else {
			rfiles[rcount++] = arg;
		}
	}

	if (do_version) {
		PrintVersion(prog_name);
	}

	if (! rcount) rfiles[rcount++] = "stdin";
	rfiles[rcount] = NULL;



	dst = (Raster *) NULL;
	for(i=0; i<rcount; i++) {
		/* Open the source file and read the header. */

		src = RasterOpen(rfiles[i], srcformat);
		if (src == (Raster *) NULL) {
			(void) RasterPrintError(rfiles[i]);
			continue;
		}

		status = RasterRead(src);
		if (status != RAS_OK) {
			(void) RasterPrintError(rfiles[i]);
			(void) RasterClose(src);
			continue;
		}

		if (!do_window) {
			src_x = 0; src_nx = src->nx;
			src_y = 0; src_ny = src->ny;
		}
		else {
			if (src_x < 0 || src_x > src->nx - 1 ||
				src_y < 0 || src_y > src->ny - 1 ||
				(src_x + src_nx - 1) > (src->nx - 1) ||
				(src_y + src_ny - 1) > (src->ny - 1)) {

				fprintf(stderr, "Window out of range\n");
				exit(1);
			}
		}

		/*
		 * if this is the first time through open the destination
		 * file. We have to do this now because we can't open
		 * the output file until we know the encoding of the input
		 * file.
		 */
		if (! dst) {
			/*
			 * if output is stdout and output format is unspecified
			 * use the input format. If thats not specified 
			 * use whatever format the first input file is
			 */
			if (! strcmp(dstfile, "stdout") && dstformat == NULL) {
				dstformat = src->format;
			}

			if (src->type == RAS_INDEXED) {
				dst = RasterOpenWrite(
					dstfile,src_nx,src_ny, Comment, 
					RAS_INDEXED, dstformat
				);
				if (dst == (Raster *) NULL) {
					(void) RasterPrintError((char *) NULL);
					(void) RasterClose(src);
					exit(1);
				}
				rasterType = RAS_INDEXED;
			}
			else {
				dst = RasterOpenWrite(
					dstfile, src_nx, src_ny, Comment, 
					RAS_DIRECT, dstformat
				);
				if (dst == (Raster *) NULL) {
					(void) RasterPrintError((char *) NULL);
					(void) RasterClose(src);
					exit(1);
				}
				rasterType = RAS_DIRECT;
			}
			
			nX = src->nx;
			nY = src->ny;
		}
		else {
			if (! (src->nx == nX && src->ny == nY)) {
				fprintf(
					stderr, 
					"Raster size changes not allowed\n"
				);
				RasterClose(src); 
				continue;
			}
			if (src->type != rasterType) {
				fprintf(
					stderr, 
					"Raster enconding changes not allowed\n"
					);
				RasterClose(src);
				continue;
			}
		}

		do {
			RasterOp(src, dst, src_x, src_y, src_nx, src_ny,0,0,0);

			status = RasterWrite(dst);
			if (status != RAS_OK) {
				(void) RasterPrintError((char *) NULL);
				exit(1);
			}

			if (do_verbose) {
				fprintf(
					stderr, "Copied frame %5d to %s\n", 
					frame++, dstfile
				);
			}
		
			status = RasterRead(src);
		} while ( status == RAS_OK );

		if (status == RAS_ERROR) {
			(void) RasterPrintError((char *) NULL);
		}
		(void) RasterClose(src);
	}
	exit(0);
}

RasterCopy(src, dst, src_x, src_y, src_nx, src_ny)
	Raster	*src;
	Raster	*dst;
	int	src_x, src_y, src_nx, src_ny;
{
	int	x, y;
	int	r, g, b;
	int	x1, y1, x2, y2;

	x1 = src_x;
	y1 = src_y;
	x2 = src_x + src_nx - 1;
	y2 = src_y + src_ny - 1;

	if (src->type == RAS_INDEXED)
		RasterCopyColormap(src, dst);
	
	for(y=y1; y<=y2; y++)
	for(x=x1; x<=x2; x++)
	{
		if (src->type == RAS_DIRECT) {
			r = DIRECT_RED(src, x, y);
			g = DIRECT_GREEN(src, x, y);
			b = DIRECT_BLUE(src, x, y);
			DIRECT_RED(dst, x, y) = r;
			DIRECT_GREEN(dst, x, y) = g;
			DIRECT_BLUE(dst, x, y) = b;
		}
		else if (src->type == RAS_INDEXED) {
			INDEXED_PIXEL(dst, x, y) = INDEXED_PIXEL(src, x, y);
		}
	}
}

/*ARGSUSED*/
RasterOp(src, dst, src_x, src_y, src_nx, src_ny, dst_x, dst_y, op)
	Raster	*src;
	Raster	*dst;
	int	src_x, src_y, src_nx, src_ny, dst_x, dst_y;
	int	op;
{
	int	r, g, b;
	int	sx, sy, dx, dy;
	int	sx1, sx2, sy1, sy2;

	if (src->type == RAS_INDEXED) {
		RasterCopyColormap(src, dst);
	}

	sx1 = src_x;
	sx2 = src_x + src_nx - 1;
	sy1 = src_y;
	sy2 = src_y + src_ny - 1;
	
	for(sy=sy1, dy=dst_y; sy<=sy2; sy++, dy++)
	for(sx=sx1, dx=dst_x; sx<=sx2; sx++, dx++)
	{
		if (src->type == RAS_DIRECT) {
			r = DIRECT_RED(src, sx, sy);
			g = DIRECT_GREEN(src, sx, sy);
			b = DIRECT_BLUE(src, sx, sy);
			DIRECT_RED(dst, dx, dy) = r;
			DIRECT_GREEN(dst, dx, dy) = g;
			DIRECT_BLUE(dst, dx, dy) = b;
		}
		else if (src->type == RAS_INDEXED) {
			INDEXED_PIXEL(dst, sx, sy) = INDEXED_PIXEL(src, dx, dy);
		}
	}
}
