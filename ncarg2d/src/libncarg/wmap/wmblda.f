C
C	$Id: wmblda.f,v 1.1 1994-09-09 23:54:41 fred Exp $
C
      BLOCKDATA WMBLDA
C
C  Initialize data.
C
      include 'wmcomn.h'
C
C  Default width for symbols along the front line (in NDC).
C
      DATA SYMWID/0.0325/
C
C  Spacings for positioning symbols on the curves. 
C
      DATA  BEGDST,ENDDST,BETDST/0.015, 0.015, 0.045/
C
C  Maximum number of symbols allowed on a front line.
C
      DATA MAXSYM/200/
C
C  Front type (1=cold, 2=warm, 3=stationary, 4=occluded.
C
      DATA IFRONT/2/
C
C  Array of symbol types initialized to all warm front symbols.
C
      DATA ISTYPE/ISDIM*2/
C
C  Current array index.
C
      DATA IARNDX/1/
C
C  Length of current curve.
C
      DATA CRVLEN/0.0/
C
C  Slope data.
C
      DATA ISLFLG,SLOPE1,SLOPE2,SLOPEL,SLOPER/3, 0.0, 0.0, 0.0, 0.0/
C
C  Flag for aloft or surface (0=surface; non-zero=aloft).
C
      DATA IALOFT/0/
C
C  Line widths when using the GKS linewidth scale factor.
C
      DATA RLINWD/8.0/
C
C  Flags method of implementing linewidths (0=GKS linewidth; 1=internal)
C
      DATA IWDTYP/0/
C
C  Line width as fraction of screen height, used when IWDTYP=1.
C
      DATA SLINWD/0.00275/
C
C  Color index for all objects.
C
      DATA ICOLOR/1/
C
C  Number of points to use for Bezier curve for warm front symbols.
C
      DATA NPTSBZ/51/
C
C  Height of the boxed weather type labels like "BREEZY", "NICE", etc.
C  plotted with WMLABW, and height of the weather type legend boxes.
C
      DATA WSIZEW/0.014/
C
C  Height of the labels for city names and daily high/low temps. 
C  plotted with WMLABC.
C
      DATA WSIZEC/0.0105/
C
C  Height of the symbols comprising the regional weather areas like
C  flurries, snow, etc.
C
      DATA WSIZER/0.008/
C
C  Height of the regional temperature labels plotted with WMLABT.
C
      DATA WSIZET/0.0165/
C
C  Height of symbols plotted with WMLABS.
C
      DATA WSIZES/0.020/
C
C  Size of arrows as a fraction of the maximum screen height.
C
      DATA ARWSIZ/0.035/
C
C  Scale factor for the tail of an arrow, independent of its size.
C
      DATA ARWLEN/1.0/
C
C  Direction of arrows, in degrees.  
C
      DATA ARWDIR/0.0/
C
C  Diameter of dots used to mark cities.
C
      DATA CDOTSZ/0.006/
C
C  Size of background margins for cities and daily temperature highs
C  and lows.
C
      DATA CTYMRG/0.002/
C
C  Size of background margins for regional temperature labels.
C
      DATA TMPMRG/0.001/
C
C  Color index to use for the backgrounds of the cities and daily
C  temperature highs and lows.
C
      DATA IBGCTY/0/
C
C  Color index to use for the characters of the regional temperature 
C  labels.
C
      DATA IFGTRG/1/
C
C  Color index to use for the dots that mark cities.
C
      DATA IDOTCO/1/
C
C  Length of windbarb shaft as a fraction of maximim screen height;
C  length of full windbarb tic as a percentage of the shaft length;
C  distance between tics along the barb; 
C  radius of larger circle drawn for calm (as percentage of shaft length); 
C  angle the barbs make with the shaft (in degrees);
C  diameter of the sky-cover symbol at base of wind barb in station model
C    (as a percentage of the shaft length).
C  flag to indicate whether the base of the windbarb should be centered
C    at the specified coordinate, or whether room should be made for
C    the sky cover circle that is drawn in the station model;
C  size of text label in the station model as a percentage of the
C    shaft length of the wind barb.
C
      DATA WBSHFT,WBFTIC,WBDIST,WBCLMR,WBBANG,IWBBAS,WBBASF,WBLSIZ
     +    / 0.035,  0.33,  0.10,  0.25,   62.,  0.30,     0,  0.17 /       
C
C  Extent of barb ticks.
C
      DATA WBXL,WBXR,WBYB,WBYT/0.,0.,0.,0./
C
      END
