C
C $Id: pcbdff.f,v 1.1 1992-11-17 18:45:57 kennison Exp $
C
      BLOCK DATA PCBDFF
C
C BLOCK DATA for filled fonts.
C
C
C COMMON PCFFME --  Font metric information for filled fonts.
C
C  TYPFLG  --  Flag indicating fontcap for filled font or stroked font.
C  CHRSTR  --  ASCII decimal equivalent for first character in the font.
C  CHREND  --  ASCII decimal equivalent for last character in the font.
C  FRIGHT  --  Maximum X value in the font coordinate system.
C  FTOP    --  Font top in font coordinate system.
C  FCAPOV  --  Font cap overshoot.
C  FCAP    --  Font cap.
C  FXHOV   --  x-height overshoot.
C  FXH     --  x-height.
C  FHALF   --  Font half.
C  FBASE   --  Font base.
C  FBOT    --  Font bottom.
C  FCHSWD  --  Font cap horizontal stem width.
C  FCVSWD  --  Font cap vertical stem width.
C  FLLX    --  Lower left X value in the font coordinate system.
C  FLLY    --  Lower left Y value in the font coordinate system.
C  FURX    --  Upper right X value in the font coordinate system.
C  FURY    --  Upper right Y value in the font coordinate system.
C  FLLEX   --  Lower left X value in the extended font coordinate system.
C  FLLEY   --  Lower left Y value in the extended font coordinate system.
C  FUREX   --  Upper right X value in the extended font coordinate system.
C  FUREY   --  Upper right Y value in the extended font coordinate system.
C  TABPNT  --  Byte pointer to the first byte of in the digitizations.
C  XBITWD  --  Bit width of X coordinates in the font coordinate system.
C  YBITWD  --  Bit width of Y coordinates in the font coordinate system.
C  XBIAS   --  Bias added to the X coordinates.
C  YBIAS   --  Bias added to the Y coordinates.
C  PKFLWD  --  Bit width of the special value flags in the packets.
C  LSTPNT  --  Byte pointer to last byte of last character digitization.
C
C
      PARAMETER (NUMNTR=29)
      COMMON /PCMTRC/TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV,
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  ,
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  ,
     +               FLLEX , FLLEY , FUREX , FUREY , TABPNT, XBITWD, 
     +               YBITWD, XBIAS , YBIAS , PKFLWD, LSTPNT
      INTEGER        TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV,
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  ,
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  ,
     +               FLLEX , FLLEY , FUREX , FUREY , TABPNT, XBITWD, 
     +               YBITWD, XBIAS , YBIAS , PKFLWD, LSTPNT
      INTEGER FNINFO(NUMNTR)
      EQUIVALENCE (FNINFO,TYPFLG)
      SAVE   /PCMTRC/
C
C  COMMON PCINDX  --  Fontcap indices, flags, and arrays.
C
C    IBFC    --  Integer array for holding the binary fontcap.
C    SFLGS   --  The special value flags for the packets in a character
C                digitization.  This array is dimensioned for the maximum
C                number of lines that would appear in tha ASCII 
C                fontcap for any character description.
C    CHRPNT  --  Byte pointers into the digitized character descriptions
C                in the binary fontcap.
C    IXC     --  Array used to store X values extracted from the fontcap. 
C    IXC     --  Array used to store Y values extracted from the fontcap. 
C    XC      --  Array for storing unbiased, recentered, and scaled X
C                coordinate values.
C    YC      --  Array for storing unbiased, recentered, and scaled Y
C                coordinate values.
C    OUTLIN  --  Flag to indicate an outline font has been requested.
C    SCALE   --  Scale factor for converting the font coordinate character
C                height to the requested character height.
C
C
      PARAMETER (IFCLEN=3000, ICLEN=150)
      COMMON /PCINDX/IBFC(IFCLEN)    ,SFLGS(ICLEN)   ,CHRPNT(128),
     +               IXC(ICLEN)      ,IYC(ICLEN)     ,XC(ICLEN)  ,
     +               YC(ICLEN)       ,OUTLIN         ,SCALE
      INTEGER        IBFC            ,SFLGS          ,CHRPNT     , 
     +               IXC             ,IYC            ,OUTLIN
      REAL           XC              ,YC             ,SCALE
      SAVE   /PCINDX/
C
C COMMON PCBZSP -- Array space for calling the Bezier curve generator.
C
C   BCNTLX -- X coordinates for Bezier control points.
C   BCNTLY -- Y coordinates for Bezier control points.
C   BZXC   -- Interpolated X coordinate values.
C   BZYC   -- Interpolated Y coordinate values.
C
C
      PARAMETER (IBZL = 129)
      COMMON /PCBZSP/BCNTLX(4), BCNTLY(4), BZXC(IBZL), BZYC(IBZL)
      REAL           BCNTLX   , BCNTLY   , BZXC      , BZYC
      SAVE   /PCBZSP/
C
      END
