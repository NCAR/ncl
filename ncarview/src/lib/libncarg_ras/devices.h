/*
 *	$Id: devices.h,v 1.5 1992-02-12 11:24:37 don Exp $
 */
typedef struct RasterDeviceStruct {
	char			*name;
	struct RasterStruct	*(*Open)();
	struct RasterStruct	*(*OpenWrite)();
	int			(*Read)();
	int			(*Write)();
	int			(*Close)();
	int			(*PrintInfo)();
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
	ClearTextRead, ClearTextWrite, ClearTextClose, ClearTextPrintInfo
},
#ifdef BuildRasterNrif
{
	"nrif", NrifOpen, NrifOpenWrite, 
	NrifRead, NrifWrite, NrifClose, NrifPrintInfo
},
#endif
#ifdef BuildRasterNrif
{
	"xwd", XWDOpen, XWDOpenWrite, 
	XWDRead, XWDWrite, XWDClose, XWDPrintInfo
},
#endif
#ifdef BuildRasterHDF
{
	"hdf", HDFOpen, HDFOpenWrite, 
	HDFRead, HDFWrite, HDFClose, HDFPrintInfo
},
#endif
#ifdef BuildRasterSun
{
	"sun", SunOpen, SunOpenWrite, 
	SunRead, SunWrite, SunClose, SunPrintInfo
},
#endif
#ifdef BuildRasterHPLJ
{
	"hplj", HPLJOpen, HPLJOpenWrite, 
	HPLJRead, HPLJWrite, HPLJClose, HPLJPrintInfo
},
#endif
#ifdef BuildRasterParallax
{
	"parallax", ParallaxOpen, ParallaxOpenWrite, 
	ParallaxRead, ParallaxWrite, ParallaxClose, ParallaxPrintInfo
},
#endif
#ifdef BuildRasterAVS
{
	"avs", AVSOpen, AVSOpenWrite, 
	AVSRead, AVSWrite, AVSClose, AVSPrintInfo
},
#endif
};

static int NumberOfDevices = (sizeof(rasdevices) / sizeof(RasterDevice));
