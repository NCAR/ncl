/*
 * $Id: rassplit.c,v 1.10 2008-07-27 03:18:41 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <ncarg/c.h>
#include <ncarg/ncarg_ras.h>

static char	*ProgramName = "rassplit";

int		OptionVerbose = False;
char		*OptionOutputFormat = (char *) NULL;
char		*OptionInputFormat = (char *) NULL;

static char	*srcfile = (char *) NULL;
static char	*Comment = "Created by rassplit";

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	i;
	char	*arg;
	int	status;
	Raster	*src, *dst, *RasterOpen(), *RasterOpenWrite();
	int	frame = 0;
	char	*out_format, *p, *root = (char *) NULL;
	static	char dstfile[256];
	static	int frame_list[1024];
	static	int frames_selected = 0;
	static	int frames_copied = 0;

	if (argc == 1) {
		Usage();
		exit(0);
	}

	(void) RasterInit(&argc, argv);

	while(--argc) {
		arg = *++argv;

		if (!strcmp(arg, "-")) {
			srcfile = "stdin";
		}
		else if (!strcmp(arg, "-help")) {
			(void) PrintHelp();
			exit(0);
		}
		/* Option for specifying output format */
		else if (!strcmp(arg, "-ofmt")) {
			if (argc > 0) {
				argc--;
				OptionOutputFormat = *++argv;
			}
			else {
				fprintf(stderr,
					"%s: Ran out of arguments\n",
					ProgramName);
				exit(1);
			}
		}
		/* Option for specifying input format */
		else if (!strcmp(arg, "-ifmt")) {
			if (argc > 0) {
				argc--;
				OptionInputFormat = *++argv;
			}
			else {
				fprintf(stderr,
					"%s: Ran out of arguments\n",
					ProgramName);
				exit(1);
			}
		}
		/* Option for specifying a list of frames to pull out */
		else if (!strcmp(arg, "-f")) {
			while(argc > 0 && isdigit(**(argv+1))) {
				argc--;
				frame_list[frames_selected++] = atoi(*++argv);
			}
		}
		else if ( !strcmp(arg, "-v") || !strcmp(arg, "-verbose") ) {
			OptionVerbose = True;
		}
		else if (!strcmp(arg, "-Version")) {
			(void) PrintVersion(ProgramName);
			exit(0);
		}
		else if (*arg == '-') {
			(void) fprintf(stderr,
				"%s: Unknown option \"%s\"\n",
				ProgramName, arg);
			exit(1);
		}
		else {
			if (srcfile == (char *) NULL)
				srcfile = arg;
			else {
				(void) fprintf(stderr, "Too many arguments\n");
				exit(1);
			}
		}
	}

	if (srcfile == (char *) NULL ) {
		(void) fprintf(stderr,"%s: Not enough arguments\n",ProgramName);
		exit(1);
	}

	root = malloc(strlen(srcfile) + 1);
	if (root == (char *) NULL) {
		(void) fprintf(stderr, "%s: Memory error\n", ProgramName);
		exit(1);
	}

	(void) strcpy(root, srcfile);
	p = strchr(root, '.');
	if (p) *p = '\0';

	/*
	** By default, the output file defaults to be the same but
	** this won't work for HDF files. Warn the user and select
	** a reasonable detour.
	*/
	if (! OptionOutputFormat) OptionOutputFormat = OptionInputFormat;

	if (OptionOutputFormat == (char *) NULL) {
		out_format = strrchr(srcfile, '.');
		if (out_format != (char *) NULL) {
			out_format++;
			if (!strcmp(out_format, "hdf")) {
				(void) fprintf(stderr,
		"%s: HDF-to-HDF output not available, use -ofmt option\n",
				ProgramName);
				exit(1);
			}
		}
	}
	else {
		out_format = OptionOutputFormat;
	}

	/* Open the source file and read the header. */

	src = RasterOpen(srcfile, OptionInputFormat);
	if (src == (Raster *) NULL) {
		(void) RasterPrintError((char *) NULL);
		exit(1);
	}

	frames_copied = 0;

	for(frame=1, frames_copied = 0; 
	    frames_selected == 0 || frames_copied < frames_selected;
	    frame++) {

		int	selected_frame;

		status = RasterRead(src);
		if (status == RAS_EOF) {
			exit(0);
		}
		else if (status != RAS_OK) {
			(void) RasterPrintError((char *) NULL);
			exit(1);
		}

		/* Prepare single file name. */

		(void) sprintf(dstfile, "%s.%4d.%s", root, frame, out_format);
		for(p=dstfile; *p!='\0'; p++) {
			if (*p == ' ') *p = '0';
		}

		if (frames_selected != 0) {
			selected_frame = False;
			for(i=0; i<frames_selected; i++) {
				if (frame == frame_list[i]) {
					selected_frame = True;
				}
			}
		}
					
		/* Open the destination file. */

		if (frames_selected == 0 || selected_frame) {
		frames_copied++;
		if (src->type == RAS_INDEXED) {
			dst = RasterOpenWrite(dstfile,src->nx,src->ny, 
				Comment, RAS_INDEXED, (char *) NULL);
			if (dst == (Raster *) NULL) {
				(void) RasterPrintError((char *) NULL);
				exit(1);
			}
		}
		else {
			dst = RasterOpenWrite(dstfile, src->nx, src->ny, 
				Comment, RAS_DIRECT, (char *) NULL);
			if (dst == (Raster *) NULL) {
				(void) RasterPrintError((char *) NULL);
				exit(1);
			}
		}

		if (src->type == RAS_INDEXED) {
			(void) RasterCopyColormap(src, dst);
		}

		RasterOp(src, dst, 0, 0, src->nx, src->ny, 0, 0, 0);

		status = RasterWrite(dst);
		if (status != RAS_OK) {
			(void) RasterPrintError((char *) NULL);
			exit(1);
		}

		status = RasterClose(dst);
		if (status != RAS_OK) {
			(void) RasterPrintError((char *) NULL);
			exit(1);
		}

		if (OptionVerbose) {
		    (void) fprintf(stderr,
			"%s: Copied frame %5d to %s\n", 
			ProgramName, frame, dstfile);
		}
		} /* end of selected_frame */
	};
}

Usage()
{
	int		i;
	static char	*msg[] = {
	"Usage: rassplit [-help] [-frames f1 f2 ...] [-ifmt input-format]",
	"                [-ofmt output-format]"
	"                [-verbose] [-Version]"
	};

	for(i=0; i<sizeof(msg)/sizeof(char *); i++) {
		(void) fprintf(stderr, "%s\n", msg[i]);
	}
}

PrintHelp()
{
	int		i;
	static char	*msg[] = {
	"rassplit - Utility which allows you to split a multi-frame",
	"           raster file into individual raster files. If the",
	"           input file has a form of root.ext, the output files",
	"           will have the form root.0001.ext, root.0002.ext, etc.",
	"",
	"Usage: rassplit options rasterfile",
	"",
	"Options:",
	"",
	"	-help			Print this help information",
	"	-v			Verbose",
	"	-Version		Print version and exit",
	"	-ifmt format-specifier	Input format selection",
	"	-ofmt format-specifier	Output format selection",
	"				(defaults to input format)",
	"	-f framenumbers		Select frames to split out",
	"				(default is all frames)",
	"",
	"Examples:",
	"",
	"	Assume you have a file called 'temp.nrif' that has",
	"	three frames.",
	"",
	"	rassplit temp.nrif",
	"",
	"	Will produce temp.0001.nrif, temp.0002.nrif, and temp.003.nrif",
	"",
	"	Assume you only want frames two and three, and the",
	"	output files should be in HDF format instead of NRIF.",
	"",
	"	rassplit -fmt hdf -f 2 3 temp.nrif"
	};

	for(i=0; i<sizeof(msg)/sizeof(char *); i++) {
		(void) fprintf(stderr, "%s\n", msg[i]);
	}
}
