.\"
.\"	$Id: ncarg_ras.l,v 1.3 1993-02-10 19:19:07 don Exp $
.\"
.\" NCAR View ncarg_ras.l 3.00 9/13/89 Clyne
.TH NCAR Raster Libraries 1NCARV "March 1992" NCAR "NCAR View 3.01"
.SH NAME
RasterInit,
RasterOpen,
RasterOpenWrite,
RasterClose,
RasterRead,
RasterWrite,
RasterPrintInfo,
RasResampleBilinear,
RasResampleNearestNeighbor,
RasterQuantize,
RasterQuantizePopular,
RasterColorToGray,
SunrasterOpen,
SunrasterOpenWrite,
SunrasterClose,
SunrasterRead,
SunrasterWrite,
NrifOpen,
NrifOpenWrite,
NrifClose,
NrifRead,
NrifWrite,
AVSOpen,
AVSOpenWrite,
AVSClose,
AVSRead,
AVSWrite,
HPLJOpen,
HPLJOpenWrite,
HPLJClose,
HPLJRead,
HPLJWrite,
XWDOpen,
XWDOpenWrite,
XWDClose,
XWDRead,
XWDWrite,
HDFOpen,
HDFOpenWrite,
HDFClose,
HDFRead,
HDFWrite,
ParallaxOpen,
ParallaxOpenWrite,
ParallaxClose,
ParallaxRead,
ParallaxWrite,

.SH SYNTAX
.nf
.B #include <ncarg_ras.h>
.PP
.B int
.B RasterResampleBilinear(src, dst, verbose)
.B Raster *src;
.B Raster *dst;
.B int verbose;
.PP
.B int
.B RasterResampleNearestNeighbor(src, dst, verbose)
.B Raster *src;
.B Raster *dst;
.B int verbose;
.PP
.B int
.B RasterRGBScale(src, scale)
.B Raster *src;
.B float scale;
.PP
.B int
.B RasterInvert(src, dst)
.B Raster	*src;
.B Raster	*dst;
.PP
.fi
.SH DESCRIPTION
.PP
Most of these functions accept and possibly return pointers
to "Raster" structures. The "Raster" structure is defined in
"ncarg_ras.h" and contains a general-purpose abstraction of
a raster structure. Current supported encoding types include
RAS_INDEXED, which is 8-bit indexed color, and RAS_DIRECT
which is 8-bit RGB color. The rasterfile access routines
RasterOpen(), RasterRead(), etc... will read files of \fImany\fP
different encodings and compression schemes, but they're all 
translated to one of the two structure for in-memory representation.
This eases the complexity of writing application software.
.IP RasterResampleNearestNeighbor(src,dst,verbose)
"src" is a pointer input variable to a source raster 
structure of any encoding type. "dst" is a pointer
to the destination raster and can be of
the same type as the source, or RGB_DIRECT.
The size of "dst" is the size of the resampled 
image. Aspect ratio is not necessarily preserved.
If "verbose" is not equal to zero, the function 
will report it's progress on stderr.
.IP
Each pixel in the output image is reverse mapped
into the continuous coordinate space of the
input image. The new pixel is simply assigned
the color of the closest input pixel. While this
is much faster than bilinear interpolation, it
can produce poor results. The advantage of
this approach over pixel replication/averaging
is that the resolution of the output image is 
not restricted to being an integer multiple or
divisor of the input image.
.IP RasterResampleBilinear(src,dst,verbose)
"src" is a pointer to a source raster structure
of any encoding type. "dst" must be a pointer
to a preallocated raster structure of type 
RAS_DIRECT. The size of "dst" is the size of
the resampled image. Aspect ratio is not necessarily
preserved. If "verbose" is not equal to zero, the 
function will report it's progress on stderr.
.IP RasterRGBScale(src, scale)
"src" is a pointer to a source raster structure
of any encoding type. The variable "scale" will be used
to scale the RGB space. R' = R * scale; G' = G * scale;
B' = B * scale.
.IP
Gives nice results but is damn slow.
.IP RasterInvert(src, dst)
Performs an source-to-destination or in-place vertical image inversion. If
\fIsrc\fP and \fIdst\fP are the same, then an in-place conversion is
done. Otherwise, The \fIsrc\fP raster is copied to the \fIdst\fP
raster with inversion taking place in the process. If \fIsrc\fP and
\fIdst\fP are not the same size or encoding, \fIsrc\fP is cropped,
centered, and re-encoded as appropriate.
.SH "SEE ALSO"
rasview(1NCARG), rascat(1NCARG), rasls(1NCARG), rassplit(1NCARG),
rasgetpal(1NCARG)
.SH BUGS
Man page is about two percent complete ;-). Interface is facing
a major redesign and documentation will change a lot.
