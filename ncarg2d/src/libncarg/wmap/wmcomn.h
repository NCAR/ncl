C
C	$Id: wmcomn.h,v 1.3 1994-09-23 17:13:44 fred Exp $
C
C
C  Size of the symbol type array.  This is a hardwired maximum.
C
      PARAMETER (ISDIM=200)
C
C  Tension factor used for spline computations.
C
      PARAMETER(TNSION=0.001)
C
C  WMCOMI contains the values for all settable INTEGER and REAL
C  parameters:
C
C    SYMWID  -  Default symbol width.
C    BEGDST  -  Distance to leave along the curve before the first
C               symbol is drawn.
C    ENDDST  -  Distance to leave along the curve after the final 
C               symbol is drawn.
C    BETDST  -  Distance to leave along the curve between symbols.
C    MAXSYM  -  Maximum number of symbols allowed along a front line.
C    ISTYPE  -  An integer array indicating the sequence of symbol
C               types to be drawn along a front line (1 = warm; 2 = cold; 
C               3 = convergence).
C    IARNDX  -  Current array index.
C    IFRONT  -  Type of front (1=warm, 2=cold, 3=occluded, 4=stationary,
C               5=squall, 6=tropical, 7=convergence).
C    CRVLEN  -  Length of current curve.
C    ISLFLG  -  Flag for usage of endline slopes in interpolator.
C    SLOPE1  -  Supplied slope for beginning of line.
C    SLOPE2  -  Supplied slope for end of line.
C    SLOPEL  -  Slope calculated at beginning of spline curve.
C    SLOPER  -  Slope calculated at end of spline curve.
C    IALOFT  -  Flag to indicate surface/aloft.
C    RLINWD  -  Line width scale factor.
C    DLINWD  -  Line width scale factor for fronts with dashed lines.
C    IWDTYP  -  Controls how line widths are implemented (0=use GKS
C               linewidth scale factor; 1=use internal subroutines)
C    SLINWD  -  Width of line if using the precise width linewidth 
C               option.
C    ICOLOR  -  GKS color index to be used for all objects.
C    NPTSBZ  -  Number of points used in calculating the Bezier curves 
C               making up the warm front symbols.
C    WSIZET  -  Size of labels drawn with WMLABT.
C    WSIZEC  -  Size of labels drawn with WMLABC.
C    WSIZEW  -  Size of labels drawn with WMLABW.
C    WSIZES  -  Size of labels drawn with WMLABS.
C    WSIZER  -  Size of symbols in regional weather patterns.
C    ARWSIZ  -  Scale factor that scales the size of an arrow uniformly.
C    ARWLEN  -  Scale factor for the length of an arrow independent of
C               its size.
C    ARWDIR  -  Direction of arrows, in degrees.
C    CDOTSZ  -  Diameter of dot used to mark cities.
C    CTYMRG  -  Margin to use for background of city/temp labels.
C    TMPMRG  -  Margin to use for background of regional temperature labels.
C    IBGCTY  -  Color index to use for background of city/temp labels.
C    IFGTRG  -  Color index to use for foreground of regional
C               temperature labels.
C    IDOTCO  -  Color index for the dots that mark cities.
C    WBSHFT  -  Length of the shaft of a wind barb as a fraction of the
C               maximum screen height.
C    WBFTIC  -  Length of a full tic on a windbarb as a percentage of 
C               the shaft length.
C    WBDIST  -  Distance between tics along a windbarb as a percentage
C               of the shaft length.
C    WBCLMR  -  Radius of the larger circle used to represent calm (as
C               a percentage of the windbarb shaft length.
C    WBBANG  -  Angle the barbs on a windbarb make with the shaft.
C    WBXL    -  Left X extent of last wind barb drawn (in NDC).
C    WBXR    -  Right X extent of last wind barb drawn (in NDC).
C    WBYB    -  Bottom Y extent of last wind barb drawn (in NDC).
C    WBYT    -  Top Y extent of last wind barb drawn (in NDC).
C    WBBASE  -  Diameter of sky cover at base of wind barb as a fraction
C               of the shaft length.
C    IWBBAS  -  Flag indicating whether space should be left at the base
C               of a wind barb (=1) or not (=0, the default).
C    WBLSIZ  -  Size of text labels in station model display.
C    ISUNC1  -  Color index for center of sun symbol.
C    ISUNC2  -  Color index for points of sun symbol.
C    ISUNC3  -  Color index for outline of cun symbol.
C    ISUNC4  -  Color index for color of sun symbol offset.
C    ICLDC1  -  Color index for cloud symbol.
C    ICLDC2  -  Color index for outline.
C    ICLDC3  -  Color index for cloud symbol offset.
C    ILTNC1  -  Color index for lightening bolt symbol.
C    ILTNC2  -  Color index for lightening bolt outline.
C    ILTNC3  -  Color index for lightening bolt offset.
C    NUMSYO  -  Forces this number of symbols on a front line if > 0.
C
      COMMON /WMCOMI/ SYMWID, BEGDST, ENDDST, BETDST, MAXSYM, IARNDX, 
     +                IFRONT, CRVLEN, ISLFLG, SLOPE1, SLOPE2, SLOPEL,
     +                SLOPER, IALOFT, RLINWD, IWDTYP, SLINWD, ICOLOR,
     +                NPTSBZ, WSIZET, WSIZEC, WSIZEW, WSIZES, WSIZER,
     +                ARWSIZ, ARWLEN, ARWDIR, CDOTSZ, CTYMRG, TMPMRG,
     +                IBGCTY, IFGTRG, IDOTCO, WBSHFT, WBFTIC, WBDIST,
     +                WBCLMR, WBBANG, WBXL  , WBXR  , WBYB  , WBYT  ,
     +                WBBASE, IWBBAS, WBLSIZ, ISUNC1, ISUNC2, ISUNC3,
     +                ISUNC4, ICLDC1, ICLDC2, ICLDC3, ILTNC1, ILTNC2,
     +                ILTNC3, DLINWD, NUMSYO,
     +                ISTYPE(ISDIM)
C
C  WMARRS contains arrray space.
C
      PARAMETER (NPTS=300,NWRK=50000)
      COMMON /WMARRS/XO(NPTS),YO(NPTS),TEMP(NPTS),S(NPTS),
     +               XS(NPTS),YS(NPTS),XOUT(NPTS),YOUT(NPTS),
     +               ALEN(NPTS),RWORK(NWRK)
