/*
 *	$Id: devices.h,v 1.8 1992-09-10 21:40:33 don Exp $
 */
typedef struct RasterDeviceStruct {
	char			*name;
	char			*description;
	struct RasterStruct	*(*Open)();
	struct RasterStruct	*(*OpenWrite)();
	int			(*Read)();
	int			(*Write)();
	int			(*Close)();
	int			(*PrintInfo)();
	int			(*ImageCount)();
} RasterDevice;

/*
If you're adding a device, you need to put a conditional with
the external declarations in, and also add the information
to the RasterDevices array, which appears below.
*/
  

extern Raster	*ClearTextOpen();
extern Raster	*ClearTextOpenWrite();
extern int	ClearTextRead();
extern int	ClearTextWrite();
extern int	ClearTextClose();
extern int	ClearTextPrintInfo();

extern int	ImageCount_();

#ifdef BuildRasterNrif
extern Raster	*NrifOpen();
extern Raster	*NrifOpenWrite();
extern int	NrifRead();
extern int	NrifWrite();
extern int	NrifClose();
extern int	NrifPrintInfo();
#endif

#ifdef BuildRasterXWD
extern Raster	*XWDOpen();
extern Raster	*XWDOpenWrite();
extern int	XWDRead();
extern int	XWDWrite();
extern int	XWDClose();
extern int	XWDPrintInfo();
#endif

#ifdef BuildRasterHDF
extern Raster	*HDFOpen();
extern Raster	*HDFOpenWrite();
extern int	HDFRead();
extern int	HDFWrite();
extern int	HDFClose();
extern int	HDFPrintInfo();
#endif

#ifdef BuildRasterSun
extern Raster	*SunOpen();
extern Raster	*SunOpenWrite();
extern int	SunRead();
extern int	SunWrite();
extern int	SunClose();
extern int	SunPrintInfo();
#endif

#ifdef BuildRasterParallax
extern Raster	*ParallaxOpen();
extern Raster	*ParallaxOpenWrite();
extern int	ParallaxRead();
extern int	ParallaxWrite();
extern int	ParallaxClose();
extern int	ParallaxPrintInfo();
#endif

#ifdef BuildRasterHPLaser
extern Raster	*HPLJOpen();
extern Raster	*HPLJOpenWrite();
extern int	HPLJRead();
extern int	HPLJWrite();
extern int	HPLJClose();
extern int	HPLJPrintInfo();
#endif

#ifdef BuildRasterAVS
extern Raster	*AVSOpen();
extern Raster	*AVSOpenWrite();
extern int	AVSRead();
extern int	AVSWrite();
extern int	AVSClose();
extern int	AVSPrintInfo();
#endif

#ifdef BuildRasterSGI
extern Raster	*SGIOpen();
extern Raster	*SGIOpenWrite();
extern int	SGIRead();
extern int	SGIWrite();
extern int	SGIClose();
extern int	SGIPrintInfo();
#endif

#ifdef BuildRasterAbekas
extern Raster	*AbekasOpen();
extern Raster	*AbekasOpenWrite();
extern int	AbekasRead();
extern int	AbekasWrite();
extern int	AbekasClose();
extern int	AbekasPrintInfo();
#endif

#ifdef BuildRasterNetcdf
extern Raster	*NetcdfOpen();
extern Raster	*NetcdfOpenWrite();
extern int	NetcdfRead();
extern int	NetcdfWrite();
extern int	NetcdfClose();
extern int	NetcdfPrintInfo();
#endif

static RasterDevice rasdevices[] = {
{
	"cleartext", "Clear Text (output only)",
	ClearTextOpen, ClearTextOpenWrite,
	ClearTextRead, ClearTextWrite, ClearTextClose, ClearTextPrintInfo,
	ImageCount_
},
#ifdef BuildRasterNrif
{
	"nrif", "NCAR Raster Interchange Format",
	NrifOpen, NrifOpenWrite, 
	NrifRead, NrifWrite, NrifClose, NrifPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterXWD
{
	"xwd", "X Window System Dump",
	XWDOpen, XWDOpenWrite, 
	XWDRead, XWDWrite, XWDClose, XWDPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterHDF
{
	"hdf", "NCSA Hierarchical Data Format",
	HDFOpen, HDFOpenWrite, 
	HDFRead, HDFWrite, HDFClose, HDFPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterSun
{
	"sun", "Sun Microsystems Rasterfile",
	SunOpen, SunOpenWrite, 
	SunRead, SunWrite, SunClose, SunPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterHPLaser
{
	"hplj", "Hewlett-Packard Laser Jet (output only)",
	HPLJOpen, HPLJOpenWrite, 
	HPLJRead, HPLJWrite, HPLJClose, HPLJPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterParallax
{
	"parallax", "Parallax VideoView VME video framebuffer",
	ParallaxOpen, ParallaxOpenWrite, 
	ParallaxRead, ParallaxWrite, ParallaxClose, ParallaxPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterAVS
{
	"avs", "Application Visualization System Rasterfile",
	AVSOpen, AVSOpenWrite, 
	AVSRead, AVSWrite, AVSClose, AVSPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterSGI
{
	"sgi", "Silicon Graphics Rasterfile",
	SGIOpen, SGIOpenWrite, 
	SGIRead, SGIWrite, SGIClose, SGIPrintInfo,
	ImageCount_
},
{
	"rgb", "Silicon Graphics Rasterfile",
	SGIOpen, SGIOpenWrite, 
	SGIRead, SGIWrite, SGIClose, SGIPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterAbekas
{
	"a60", "Abekas A60 Rasterfile",
	AbekasOpen, AbekasOpenWrite, 
	AbekasRead, AbekasWrite, AbekasClose, AbekasPrintInfo,
	ImageCount_
},
{
	"yuv", "Abekas A60 Rasterfile",
	AbekasOpen, AbekasOpenWrite, 
	AbekasRead, AbekasWrite, AbekasClose, AbekasPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterNetcdf
{
	"cdf", "Netcdf GOES image file",
	NetcdfOpen, NetcdfOpenWrite, 
	NetcdfRead, NetcdfWrite, NetcdfClose, NetcdfPrintInfo,
	ImageCount_
}
#endif
};

static int NumberOfDevices = (sizeof(rasdevices) / sizeof(RasterDevice));
