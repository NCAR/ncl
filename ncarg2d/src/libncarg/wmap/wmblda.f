C
C	$Id: wmblda.f,v 1.18 2010-01-05 03:52:14 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA WMBLDAX
C
C  Initialize data.
C
      include 'wmcomn.h'
C
C  Default line widths for drawing wind barbs.
C
      DATA BRBLWD/1.0/
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
C  User override for number of symbols on a front line.
C
      DATA NUMSYO/-1/
C
C  Front type (1=cold, 2=warm, 3=stationary, 4=occluded, 5=instability,
C              6=intertropical, 7=convergence).
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
C  Line widths for fronts when using the GKS linewidth scale factor;
C  line widths for squalls, tropical fronts, and convergence.
C
      DATA RLINWD,DLINWD/8.0,2.0/
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
C  Background color index for city names.
C
      DATA IBGCTY/0/
C
C  Color index for regional weather labels and regioanl temperature labels.
C
      DATA IFGTRG/1/
C
C  Flag for NCL to know if it is being used in conjunction with
C  mapping tools in certain applications.
C
      DATA IEZFLG/-1/
C
C  Colors for regional temperature label shadows and outlines.
C
      DATA IRLOUC,IRLBKC,IRLLSC/-1,-1,-1/
C
C  Colors of dot drawn for cities and its background.
      DATA IDOTBG,IDOTCO/0,1/
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
      DATA WBSHFT,WBFTIC,WBDIST,WBCLMR,WBBANG,WBBASE,IWBBAS,WBLSIZ
     +    / 0.035,  0.33,  0.10,  0.25,   62.,  0.30,     0,  0.17 /       
C
C  Wind barb direction (=0 away from wind; =1 toward the wind).
C
      DATA IWBDIR/0/
C
C  Extent of barb ticks.
C
      DATA WBXL,WBXR,WBYB,WBYT/0.,0.,0.,0./
C
C  Scale limits for vectors.  A vector of length VCUREF will
C  be scaled to NDC length VCNREF.
C
      DATA VCUREF,VCNREF/10.,0.02/
C
C  NDC length of vector arrow head line.
C
      DATA VCHSIZ/0.02/
C
C  Linewidth scale factor for vectors.
C
      DATA VCLWID/1.0/
C
C  Vector color.
C
      DATA VCCOLR/1/
C
C  Vector direction.
C
      DATA VCWDIR/0./
C
C  Vector size.
C
      DATA VCSIZE/10./
C
C  Angle between vector head and tail.
C
      DATA VHDANG/25./
C
C  Foreground color for vector label box.
C
      DATA VLBLFC/1/
C
C  Background color for vector label box.
C
      DATA VLBLBC/0/
C
C  Color indices for sun, cloud, and lightening bolt symbols.
C
      DATA ISUNC1, ISUNC2, ISUNC3, ISUNC4, ICLDC1, ICLDC2, ICLDC3, 
     +     ILTNC1, ILTNC2, ILTNC3
     +   /      2,      3,      1,      1,      2,      1,      1, 
     +          2,      1,      1                                 /
C
C  Color indices for warm front and cold front symbols.
C
      DATA IWARMC,ICOLDC/1,1/
C
C  Color indices for high and low symbols.
C
      DATA IHIGC1,IHIGC2,IHIGC3,IHIGC4/1,0,1,1/
      DATA ILOWC1,ILOWC2,ILOWC3,ILOWC4/0,1,0,1/
C
C  Color indices for regional weather label boxes.
C
      DATA IRGLC1,IRGLC2,IRGLC3,IRGLC4,IRGLC5/1,0,1,1,-1/
C
C  Color indices for tropical front alternate segments in dashed line.
C
      DATA ITRO1C,ITRO2C/1,1/
C
C  Arrow color, arrow shadow color, arrow outline color.
C
      DATA IAROWC,IARSHC,IAROUC/1,-1,-1/
C
C  Tension factor for spline routines, a negative value indicates that
C  the value has not been specified and a defualt value will be supplied
C  at run time.
C
      DATA TNSION/-1./
C
C  Smoothing parameter for the smoothing spline routines, a negative 
C  value indicates that the value has not been specified and a defualt 
C  value will be supplied at run time.
C
      DATA RSMOTH/-1./
C
C  Observational weights for data for smoothing spline routines.  A negative
C  value indicates that the value has not been specified and a defualt 
C  value will be supplied at run time.
C
      DATA OBSERR/-1./
C
C  Flags whether interpolation (=0) or smoothing (=1) is done.
C
      DATA ISMOTH/0/
C
C  Flags whether to use imperial units (as per the NOAA chart),
C  or metric units (as per SYNOP Data Format (FM-12) as in
C
C    http://weather.unisys.com/wxp/Appendices/Formats/SYNOP.html
C
      DATA IUNITS/0/
C
C  If IVVCOD equals 1, then plot the surface visibility as per
C  the raw two-character SYNOP codes, otherwise convert and plot
C  as mph or km as per the setting of IUNITS.
C
      DATA IVVCOD/0/
C
      END
