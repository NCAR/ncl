/*
 *	$Id: ved-par.c,v 1.1 1992-02-26 00:40:07 clyne Exp $
 */
#include	<stdio.h>
#include	<fcntl.h>
#include	<sys/types.h>
#include	<sys/mman.h>
#include 	<pixrect/pixrect_hs.h>
#include 	<errno.h>
#include 	<pixrect/tv1var.h>
#include	<string.h>
#include	"/sys/sun/tvio.h"
#include	"raster.h"
#include	"parallax.h"

extern int	OptionDebug;
extern int	OptionCenter;

static int	fd;
static Parallax	*pvp;
static int	vmefd = -1;

#ifdef STANDALONE
int		OptionDebug = 0;
char		*ProgramName;

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	i;
	char	*ext;

	ProgramName = argv[0];

#define DEFAULT_FILE	"/red/u1/don/src/conpack-terrain/lglobe.xwd"

	ParallaxOpen();
	ParallaxInit();
	ParallaxClear(143, 143, 178);
	switch (argc)
	{
		case 1:
			ParallaxLoadXWD(DEFAULT_FILE);
			break;
		case 2:
			if ( (ext = strrchr(argv[1], '.')) == (char *) NULL) {
				ParallaxLoadXWD(DEFAULT_FILE);
			}
			else if (!strcmp(ext, ".xwd")) {
				ParallaxLoadXWD(argv[1]);
			}
			else {
			}
			break;
	}
	ParallaxClose();
}
#endif STANDALONE

ParallaxInit()
{
	static int	arg;
	int		status;

	/* Sync the frame buffer */
	arg = 0;
	if ( (status = ioctl(fd, TVIOSYNC, &arg)) == -1) {
		perror("TVIO Sync");
		exit(1);
	}

	/* Input video will be NTSC */
	arg = TVIO_NTSC;
	if ( (status = ioctl(fd, TVIOSFORMAT, &arg)) == -1) {
		perror("TVIO Set Format");
		exit(1);
	}

	/* Set compression ration to be 1 to 1 */
	arg = 1;
	if ( (status = ioctl(fd, TVIOSCOMPRESS, &arg)) == -1) {
		perror("TVIO Set Compression Ratio");
		exit(1);
	}

	/* Direct video to video monitor, rather than Sun screen */
	arg = TVIO_VIDEOOUT;
	if ( (status = ioctl(fd, TVIOSOUT, &arg)) == -1) {
		perror("TVIO Set Video Destination");
		exit(1);
	}

	/* Output is RS-170A, not YUV */
	arg = TVIO_RGB;
	if ( (status = ioctl(fd, TVIOSCOMPOUT, &arg)) == -1) {
		perror("TVIO Set Component Out");
		exit(1);
	}

	/* Set Genlock on */
	arg = TRUE;
	if ( (status = ioctl(fd, TVIOSGENLOCK, &arg)) == -1) {
		perror("TVIO Set Genlock");
		exit(1);
	}

	/* Set Genlock to sync on composite sync input */
	arg = TVIO_NTSC;
	if ( (status = ioctl(fd, TVIOSSYNC, &arg)) == -1) {
		perror("TVIO Set Genlock to use NTSC in");
		exit(1);
	}

	/* Set Chroma Demodulation to automatic */
	arg = TVIO_AUTO;
	if ( (status = ioctl(fd, TVIOSSYNC, &arg)) == -1) {
		perror("TVIO Set Genlock to use NTSC in");
		exit(1);
	}

	/* Sync the frame buffer */
	arg = 0;
	if ( (status = ioctl(fd, TVIOSYNC, &arg)) == -1) {
		perror("TVIO Sync");
		exit(1);
	}
}

ParallaxPrintStatus()
{
	static int	arg;
	static int	format;
	static int	out;
	static int	compout;
	int		status;

	if ( (status = ioctl(fd, TVIOGFORMAT, &format)) == -1) {
		perror("TVIO Get Format");
	}

	if ( (status = ioctl(fd, TVIOGOUT, &out)) == -1) {
		perror("TVIO Get Video Destination");
	}

	if ( (status = ioctl(fd, TVIOGCOMPOUT, &compout)) == -1) {
		perror("TVIO Get Component Out");
	}

	(void) fprintf(stderr, "Input Format:      %s\n", format_names[format]);
	(void) fprintf(stderr, "Video Destination: %s\n", out_names[out]);
	(void) fprintf(stderr, "Component Out:     %s\n",format_names[compout]);
}
	
ParallaxOpen()
{
	if (vmefd > 0) return(0);

	if ( (fd = open(VFB_DEVICE, O_RDWR)) == -1) {
		perror(VFB_DEVICE);
		exit(1);
	}

	if ( (vmefd = open(VFB_VME_DEVICE, O_RDWR)) == -1) {
		perror(VFB_VME_DEVICE);
		return (1);
	}

	pvp = (Parallax *) mmap( (caddr_t) 0, 0x400000, PROT_READ|PROT_WRITE,
		MAP_SHARED, vmefd, VFB_VME_ADDRESS);

	if (pvp < (Parallax *) 0) {
		perror("Memory map for Parallax video frame buffer");
		exit(1);
	}

	return (0);
}

ParallaxLoadXWD(name)
	char	*name;
{
	int	status;
	Raster	*raster, *XWDOpen();
	int	i;

	if ( (raster = XWDOpen(name)) == (Raster *) NULL) {
		(void) fprintf(stderr, "Error in opening %s\n", name);
	}

	if ( (status = XWDRead(raster)) != 1 ) {
		(void) fprintf(stderr, "Bad read on %s\n", name);
	}

	ParallaxLoadRaster(raster, 1);
	XWDClose(raster);
}

ParallaxLoadRaster(src_raster, scale)
	Raster	*src_raster;
	int	scale;
{
	Raster		*RasScale();
	Raster		*RasAverage();
	Raster		*raster;
	Raster		*temp;
	int		status;
	long		word;
	int		sx, sy, dx, dy;		/* source and dest indices */
	int		src_x, src_y;		/* source upper-left corner */
	int		src_nx, src_ny;		/* source extent */
	int		dst_x, dst_y;		/* dest upper-left corner */
	int		vfb_width, vfb_height;	/* video frame buffer extent */
	int		i, r, g, b, pixel;
	static long	colormap_lwd[256];

#ifdef DEAD
	temp = RasScale(src_raster, scale);
	raster = RasAverage(temp, 3);
#endif DEAD

	raster = src_raster;

	vfb_width = VFB_WIDTH;
	vfb_height = VFB_HEIGHT - VFB_Y_OFFSET;

	if (raster->type  == RAS_INDEXED) {
		for (i=0; i<256; i++)
			colormap_lwd[i] = 
				raster->red[i] | 
				raster->green[i]<<8 | 
				raster->blue[i]<<16;
	}

	/* Set defaults for image positioning. */

	src_x = 0; src_y = VFB_Y_OFFSET;
	src_nx = raster->nx; src_ny = raster->ny;
	dst_x = 0; dst_y = 0;

	/* Calculate X mapping */

	if (raster->nx > vfb_width) {
		dst_x = 0;
		src_nx = vfb_width;
		if (OptionCenter)
			src_x = (raster->nx - vfb_width) / 2;
		else
			src_x = 0;
	}
	else {
		src_x = 0;
		src_nx = raster->nx;
		if (OptionCenter)
			dst_x = (vfb_width - src_nx) / 2;
		else
			dst_x = 0;
	}

	/* Calculate Y mapping */

	if (raster->ny >= vfb_height) {
		dst_y = 0;
		src_ny = vfb_height;
		if (OptionCenter)
			src_y = (raster->ny - vfb_height) / 2;
		else
			src_y = 0;
	}
	else {
		src_y = 0;
		src_ny = raster->ny;
		if (OptionCenter)
			dst_y = (vfb_height - src_ny) / 2;
		else
			dst_y = 0;
	}

	for(sy=src_y, dy=dst_y + VFB_Y_OFFSET; sy<src_y+src_ny-1; sy++, dy++) {
		for(sx=src_x, dx=dst_x; sx<src_x+src_nx-1; sx++, dx++) {
			if (raster->type  == RAS_INDEXED) {
			  pixel  = INDEXED_PIXEL(raster, sx, sy);
			  pvp->fb.line[dy].pixel[dx].lwd = 
			    colormap_lwd[pixel];
			}
			else if (raster->type == RAS_DIRECT) {
			  pvp->fb.line[dy].pixel[dx].lwd =
				DIRECT_RED(raster, sx, sy) | 
				DIRECT_GREEN(raster, sx, sy) <<8 | 
				DIRECT_BLUE(raster, sx, sy) << 16;
			}
		}
	}
}

ParallaxClear(r, g, b)
	unsigned char r,g,b;
{
	int	row, col;
	int	count;
	long	word;

	word = 0;
	word = r | (g << 8) | (b << 16);

	for(row=0; row<VFB_HEIGHT; row++)
	for(col=0; col<VFB_WIDTH; col++) {
		pvp->fb.line[row].pixel[col].lwd = word;
	}
}

ParallaxClearRegion(r, g, b, x1, x2, y1, y2)
	unsigned char r,g,b;
{
	int	row, col;
	int	count;
	long	word;

	word = 0;
	word = r | (g << 8) | (b << 16);

	if (x1 < 0) x1 = 0;
	if (x2 > VFB_WIDTH - 1) x2  = VFB_WIDTH - 1;
	if (y1 < 0) y1 = 0;
	if (y2 > VFB_HEIGHT - 1) y2  = VFB_HEIGHT - 1;


	for(row=y1; row<=y2; row++)
	for(col=x1; col<=x2; col++) {
		pvp->fb.line[row].pixel[col].lwd = word;
	}
}

ParallaxClose()
{
	(void) close(vmefd);
}

#ifdef DEAD
ParallaxLoadFile(filename)
	char	*filename;
{
	int		i;
	struct pixrect *src_pixrect;
	colormap_t	colormap;
	FILE           *file;

	if (!(file = fopen(filename, "r"))) {
		perror("open failed:");
		return (errno);
	}

	src_pixrect = pr_load(file, &colormap);

	(void) fclose(file);

	if (!src_pixrect) {
		(void) fprintf(stderr, "Error reading rasterfile data\n");
		return;
	}
	ParallaxLoadPixrect(src_pixrect, colormap);
}

ParallaxLoadPixrect(src_pixrect, colormap)
	struct pixrect	*src_pixrect;
	colormap_t	colormap;
{
    struct pr_pos	src, dst;
    int			width, height;
    struct mpr_data	*mprdata;
    unsigned char	*mprimage;
    unsigned char	*ip;
    long		colormap_lwd[256];

    mprdata = (struct mpr_data *) src_pixrect->pr_data;
    mprimage = (unsigned char *) mprdata->md_image;

    src.x = src.y = dst.x = dst.y = 0;
    width = src_pixrect->pr_width;
    height = src_pixrect->pr_height;
    if (src_pixrect->pr_height > VFB_HEIGHT) {
	src.y = (src_pixrect->pr_height - VFB_HEIGHT) / 2;
	height = VFB_HEIGHT;
    } else if (src_pixrect->pr_height <= 
    		(VFB_HEIGHT-VFB_Y)) {
    	/* 
    	 * This offsets the image so that it will show all of it 
    	 */
    	dst.y = VFB_Y + (VFB_HEIGHT - 
    		(src_pixrect->pr_height+VFB_Y)) / 2;
    	height = dst.y + src_pixrect->pr_height;
#ifdef DEAD
	/* clear the top */
	pr_rop(dst_pixrect, 0, 0, VFB_WIDTH, dst.y, 
    		PIX_CLR, NULL, 0, 0);
	/* now bottom */
	pr_rop(dst_pixrect, 0, height, VFB_WIDTH, 
    		(VFB_HEIGHT - height), PIX_CLR, NULL, 0, 0);
#endif DEAD
    } else if( (src_pixrect->pr_height > VFB_HEIGHT) &&
    	(src_pixrect->pr_height > (VFB_HEIGHT-VFB_Y) )) {
    	dst.y = VFB_HEIGHT - src_pixrect->pr_height;
    	height = dst.y + src_pixrect->pr_height;
    }

    if (src_pixrect->pr_width > VFB_WIDTH) {
	src.x = (src_pixrect->pr_width - VFB_WIDTH) / 2;
	width = VFB_WIDTH;
    } else if (src_pixrect->pr_width < VFB_WIDTH) {
	dst.x = (VFB_WIDTH - src_pixrect->pr_width) / 2;
	width = dst.x + src_pixrect->pr_width;
#ifdef DEAD
	/* clear left side  */
	pr_rop(dst_pixrect, 0, dst.y, dst.x,
    		src_pixrect->pr_height, PIX_CLR, NULL, 0, 0);
	/* and the right */
	pr_rop(dst_pixrect, width , dst.y,
	       VFB_WIDTH - width,
	       src_pixrect->pr_height, PIX_CLR, NULL, 0, 0);
#endif DEAD
    }

    if (src_pixrect->pr_depth == 8) {
	/* we need to convert it to 24 */
	register int    x, y, sx, sy, i;

	if ( (colormap.type != RT_STANDARD) || (colormap.length > 256)) {
	    (void) fprintf(stderr,
		"Sorry, this type of colormap is not supported\n");
	    pr_destroy(src_pixrect);
	    return;
	}

	/* Precompute each color triple for speed */
	for (i=0; i<256; i++)
		colormap_lwd[i] = 
			colormap.map[0][i] |
			colormap.map[1][i] << 8 |
			colormap.map[2][i] << 16 ;

	for (y = dst.y, sy = src.y, ip = mprimage; y < height; y++, sy++) {
	    for (x = dst.x, sx = src.x; x < width; x++, sx++) {
		pvp->fb.line[y].pixel[x].lwd = colormap_lwd[ip[x]];
#ifdef DEAD
		i = ip[x];
		pvp->fb.line[y].pixel[x].lwd = 
			colormap.map[0][i] |
			colormap.map[1][i] << 8 |
			colormap.map[2][i] << 16 ;
#endif DEAD
	    }
	    ip += mprdata->md_linebytes;
	}
    } else {
	register int    x, y, sx, sy, i;
	for (y = dst.y, sy = src.y; y < height; y++, sy++) {
	    for (x = dst.x, sx = src.x; x < width; x++, sx++) {
		i = pr_get(src_pixrect, sx, sy);
		pvp->fb.line[y].pixel[x].rgb.r = colormap.map[0][i];
		pvp->fb.line[y].pixel[x].rgb.g = colormap.map[1][i];
		pvp->fb.line[y].pixel[x].rgb.b = colormap.map[2][i];
	    }
	}
#ifdef DEAD
	pr_rop(dst_pixrect, dst.x, dst.y, width, height, PIX_SRC,
	       src_pixrect, src.x, src.y);
#endif DEAD
    }
#ifdef DEAD
    pr_destroy(src_pixrect);
#endif DEAD
    return (0);

}

ParallaxLoad(filename)
    char *filename;
{
    struct pixrect *src_pixrect;
    FILE           *file;
    colormap_t     colormap;
    struct pr_pos   src, dst;
    int             width, height;
    struct mpr_data	*mprdata;
    unsigned char	*mprimage;
    unsigned char	*ip;

    if (!(file = fopen(filename, "r"))) {
	perror("open failed:");
	return (errno);
    }
    src_pixrect = pr_load(file, &colormap);
    mprdata = (struct mpr_data *) src_pixrect->pr_data;
    mprimage = (unsigned char *) mprdata->md_image;
    (void) fclose(file);
    if (!src_pixrect) {
	(void) fprintf(stderr, "Error reading rasterfile data\n");
	return;
    }
    src.x = src.y = dst.x = dst.y = 0;
    width = src_pixrect->pr_width;
    height = src_pixrect->pr_height;
    if (src_pixrect->pr_height > VFB_HEIGHT) {
	src.y = (src_pixrect->pr_height - VFB_HEIGHT) / 2;
	height = VFB_HEIGHT;
    } else if (src_pixrect->pr_height <= 
    		(VFB_HEIGHT-VFB_Y)) {
    	/* 
    	 * This offsets the image so that it will show all of it 
    	 */
    	dst.y = VFB_Y + (VFB_HEIGHT - 
    		(src_pixrect->pr_height+VFB_Y)) / 2;
    	height = dst.y + src_pixrect->pr_height;
#ifdef DEAD
	/* clear the top */
	pr_rop(dst_pixrect, 0, 0, VFB_WIDTH, dst.y, 
    		PIX_CLR, NULL, 0, 0);
	/* now bottom */
	pr_rop(dst_pixrect, 0, height, VFB_WIDTH, 
    		(VFB_HEIGHT - height), PIX_CLR, NULL, 0, 0);
#endif DEAD
    } else if( (src_pixrect->pr_height > VFB_HEIGHT) &&
    	(src_pixrect->pr_height > (VFB_HEIGHT-VFB_Y) )) {
    	dst.y = VFB_HEIGHT - src_pixrect->pr_height;
    	height = dst.y + src_pixrect->pr_height;
    }

    if (src_pixrect->pr_width > VFB_WIDTH) {
	src.x = (src_pixrect->pr_width - VFB_WIDTH) / 2;
	width = VFB_WIDTH;
    } else if (src_pixrect->pr_width < VFB_WIDTH) {
	dst.x = (VFB_WIDTH - src_pixrect->pr_width) / 2;
	width = dst.x + src_pixrect->pr_width;
#ifdef DEAD
	/* clear left side  */
	pr_rop(dst_pixrect, 0, dst.y, dst.x,
    		src_pixrect->pr_height, PIX_CLR, NULL, 0, 0);
	/* and the right */
	pr_rop(dst_pixrect, width , dst.y,
	       VFB_WIDTH - width,
	       src_pixrect->pr_height, PIX_CLR, NULL, 0, 0);
#endif DEAD
    }

    if (src_pixrect->pr_depth == 8) {
	/* we need to convert it to 24 */
	register int    x, y, sx, sy, i;

	if ( (colormap.type != RT_STANDARD) || (colormap.length > 256)) {
	    (void) fprintf(stderr,
			"Sorry, this type of colormap is not supported\n");

	    pr_destroy(src_pixrect);
	    return;
	}
	for (y = dst.y, sy = src.y, ip = mprimage; y < height; y++, sy++) {
	    for (x = dst.x, sx = src.x; x < width; x++, sx++) {
		/* i = pr_get(src_pixrect, sx, sy); */
		i = ip[x];
		pvp->fb.line[y].pixel[x].lwd = 
			colormap.map[0][i] |
			colormap.map[1][i] << 8 |
			colormap.map[2][i] << 16 ;

#ifdef DEAD
		pvp->fb.line[y].pixel[x].rgb.r = colormap.map[0][i];
		pvp->fb.line[y].pixel[x].rgb.g = colormap.map[1][i];
		pvp->fb.line[y].pixel[x].rgb.b = colormap.map[2][i];
#endif DEAD
	    }
	    ip += mprdata->md_linebytes;
	}
    } else {
	register int    x, y, sx, sy, i;
	for (y = dst.y, sy = src.y; y < height; y++, sy++) {
	    for (x = dst.x, sx = src.x; x < width; x++, sx++) {
		i = pr_get(src_pixrect, sx, sy);
		pvp->fb.line[y].pixel[x].rgb.r = colormap.map[0][i];
		pvp->fb.line[y].pixel[x].rgb.g = colormap.map[1][i];
		pvp->fb.line[y].pixel[x].rgb.b = colormap.map[2][i];
	    }
	}
#ifdef DEAD
	pr_rop(dst_pixrect, dst.x, dst.y, width, height, PIX_SRC,
	       src_pixrect, src.x, src.y);
#endif DEAD
    }
    pr_destroy(src_pixrect);
    return (0);
}
#endif DEAD
