typedef struct RasterDeviceStruct {
	char			*name;
	int			(*Probe)();
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
  

extern int	ClearTextProbe();
extern Raster	*ClearTextOpen();
extern Raster	*ClearTextOpenWrite();
extern int	ClearTextRead();
extern int	ClearTextWrite();
extern int	ClearTextClose();
extern int	ClearTextPrintInfo();

#ifdef BuildRasterNrif
extern int	NrifProbe();
extern Raster	*NrifOpen();
extern Raster	*NrifOpenWrite();
extern int	NrifRead();
extern int	NrifWrite();
extern int	NrifClose();
extern int	NrifPrintInfo();
#endif

#ifdef BuildRasterNrif
extern int	XWDProbe();
extern Raster	*XWDOpen();
extern Raster	*XWDOpenWrite();
extern int	XWDRead();
extern int	XWDWrite();
extern int	XWDClose();
extern int	XWDPrintInfo();
#endif

#ifdef BuildRasterHDF
extern int	HDFProbe();
extern Raster	*HDFOpen();
extern Raster	*HDFOpenWrite();
extern int	HDFRead();
extern int	HDFWrite();
extern int	HDFClose();
extern int	HDFPrintInfo();
#endif

#ifdef BuildRasterSun
extern int	SunProbe();
extern Raster	*SunOpen();
extern Raster	*SunOpenWrite();
extern int	SunRead();
extern int	SunWrite();
extern int	SunClose();
extern int	SunPrintInfo();
#endif

#ifdef BuildRasterParallax
extern int	ParallaxProbe();
extern Raster	*ParallaxOpen();
extern Raster	*ParallaxOpenWrite();
extern int	ParallaxRead();
extern int	ParallaxWrite();
extern int	ParallaxClose();
extern int	ParallaxPrintInfo();
#endif

#ifdef BuildRasterHPLJ
extern int	HPLJProbe();
extern Raster	*HPLJOpen();
extern Raster	*HPLJOpenWrite();
extern int	HPLJRead();
extern int	HPLJWrite();
extern int	HPLJClose();
extern int	HPLJPrintInfo();
#endif

static RasterDevice rasdevices[] = {
{
	"cleartext", ClearTextProbe, ClearTextOpen, ClearTextOpenWrite,
	ClearTextRead, ClearTextWrite, ClearTextClose, ClearTextPrintInfo
},
#ifdef BuildRasterNrif
{
	"nrif", NrifProbe, NrifOpen, NrifOpenWrite, 
	NrifRead, NrifWrite, NrifClose, NrifPrintInfo
},
#endif
#ifdef BuildRasterNrif
{
	"xwd", XWDProbe, XWDOpen, XWDOpenWrite, 
	XWDRead, XWDWrite, XWDClose, XWDPrintInfo
},
#endif
#ifdef BuildRasterHDF
{
	"hdf", HDFProbe, HDFOpen, HDFOpenWrite, 
	HDFRead, HDFWrite, HDFClose, HDFPrintInfo
},
#endif
#ifdef BuildRasterSun
{
	"sun", SunProbe, SunOpen, SunOpenWrite, 
	SunRead, SunWrite, SunClose, SunPrintInfo
},
#endif
#ifdef BuildRasterHPLJ
{
	"hplj", HPLJProbe, HPLJOpen, HPLJOpenWrite, 
	HPLJRead, HPLJWrite, HPLJClose, HPLJPrintInfo
},
#endif
#ifdef BuildRasterParallax
{
	"parallax", ParallaxProbe, ParallaxOpen, ParallaxOpenWrite, 
	ParallaxRead, ParallaxWrite, ParallaxClose, ParallaxPrintInfo
},
#endif
};

static int NumberOfDevices = (sizeof(rasdevices) / sizeof(RasterDevice));
