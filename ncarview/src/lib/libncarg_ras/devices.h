/*
 *	$Id: devices.h,v 1.6 1992-03-23 21:44:59 clyne Exp $
 */
typedef struct RasterDeviceStruct {
	char			*name;
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

static RasterDevice rasdevices[] = {
{
	"cleartext", ClearTextOpen, ClearTextOpenWrite,
	ClearTextRead, ClearTextWrite, ClearTextClose, ClearTextPrintInfo,
	ImageCount_
},
#ifdef BuildRasterNrif
{
	"nrif", NrifOpen, NrifOpenWrite, 
	NrifRead, NrifWrite, NrifClose, NrifPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterNrif
{
	"xwd", XWDOpen, XWDOpenWrite, 
	XWDRead, XWDWrite, XWDClose, XWDPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterHDF
{
	"hdf", HDFOpen, HDFOpenWrite, 
	HDFRead, HDFWrite, HDFClose, HDFPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterSun
{
	"sun", SunOpen, SunOpenWrite, 
	SunRead, SunWrite, SunClose, SunPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterHPLJ
{
	"hplj", HPLJOpen, HPLJOpenWrite, 
	HPLJRead, HPLJWrite, HPLJClose, HPLJPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterParallax
{
	"parallax", ParallaxOpen, ParallaxOpenWrite, 
	ParallaxRead, ParallaxWrite, ParallaxClose, ParallaxPrintInfo,
	ImageCount_
},
#endif
#ifdef BuildRasterAVS
{
	"avs", AVSOpen, AVSOpenWrite, 
	AVSRead, AVSWrite, AVSClose, AVSPrintInfo,
	ImageCount_
},
#endif
};

static int NumberOfDevices = (sizeof(rasdevices) / sizeof(RasterDevice));
