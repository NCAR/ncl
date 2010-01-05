C
C	$Id: wmstnm.f,v 1.17 2010-01-05 03:52:14 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMSTNM(X,Y,IMDAT)
C
C  This subroutine plots station model data as per the NOAA/WMO
C  guidelines.  (X,Y) is a world coordinate specifying the center
C  of the tail of the windbarb shaft.  IMDAT is a CHARACTER*5 array 
C  of length 9 that contains the coded model data.  The first member 
C  of the  IMDAT array contains IR,IX,H,VV (in that order), and the 
C  second member contains N,DD,FF (in that order).  The remaining members
C  of IMDAT contain, as their first character, a group identifier.
C
C  In a little more detail:
C
C     IMDAT(1)( 1: 1) = iR - precipitation data indictor.
C     IMDAT(1)( 2: 2) = iX - weather data and station type indicator. 
C     IMDAT(1)( 3: 3) =  h - height above ground of base lowest cloud.
C     IMDAT(1)( 4: 5) = VV - visibility in miles and fractions for
C                            imperial units (UNT=0), or kilometers
C                            in metric units (UNT=1).
C
C     IMDAT(2)( 1: 1) =  N - total amount of cloud cover.
C     IMDAT(2)( 2: 3) = dd - direction from which wind is blowing.
C     IMDAT(2)( 4: 5) = ff - wind speed in knots.
C
C     For I=3,9 if IMDAT(I)(1:1) =
C
C             '1', then IMDAT(I)(2:2) =  sn - sign of temperature
C                       IMDAT(I)(3:5) = TTT - current air temperature
C
C             '2', then IMDAT(I)(2:2) =  sn - sign of temperature
C                       IMDAT(I)(3:5) =  Td - dew point
C
C             '3', then IMDAT(I)(2:5) =  PO - station pressure
C
C             '4', then IMDAT(I)(2:5) =  PPPP - pressure reduced to sea level
C
C             '5', then IMDAT(I)(2:2) =   a - characteristic of barograph trace
C                       IMDAT(I)(3:5) = ppp - pressure change, last 3 hrs.
C
C             '6', then IMDAT(I)(2:4) = RRR - precipitation
C                       IMDAT(I)(5:5) =  tr - time duration of precipitation
C
C             '7', then IMDAT(I)(2:3) =  ww - present weather
C                       IMDAT(I)(4:4) =  W1 - most significant past weather
C                       IMDAT(I)(5:5) =  W2 - 2nd most significant past weather
C
C             '8', then IMDAT(I)(2:2) =  Nh - fraction of sky cover
C                       IMDAT(I)(3:3) =  CL - cloud type, low clouds
C                       IMDAT(I)(4:4) =  CM - cloud type, medium clouds
C                       IMDAT(I)(5:5) =  CH - cloud type, high clouds
C
      include 'wmcomn.h'
C
      INTEGER WMGTLN
      PARAMETER (NFIELD=10,NPARMS=25)
      INTEGER WDATA(NPARMS)
C
C  Variables for the encoded fields.
C
C    IR   - Precipitation data indicator.
C    IX   - Weather data and station type indicator.
C    H    - Height above ground of lowest cloud.
C    VV   - Visibility in miles and fractions.
C    N    - Amount of cloud cover.
C    DD   - Wind direction.
C    FF   - Wind speed in knots.
C    SN1  - Sign of the ambient temperature.
C    TTT  - Current air temperature.
C    SN2  - Sign of temperature of the dew point.
C    TD   - Temperature of the dew point.
C    PO   - Station pressure (not plotted).
C    PPPP - Baromatric pressure.
C    A    - Barometric pressure tendency.
C    PPP  - Pressure change in last three hours.
C    RRR  - Amount of precipitation in last six hours.
C    TR   - Time period for the precipitation amount.
C    WW   - Present weather.
C    W1   - Most significant past weather.
C    W2   - Second most significant past weather.
C    NH   - Fraction of the sky covered by low or middle clouds.
C    CL   - Cloud type for low clouds.
C    CM   - Cloud type for medium clouds.
C    CH   - Cloud type for high clouds.
C
      COMMON /WMSLOC/ IR,IX,H,VV,N,DD,FF,SN1,TTT,SN2,TD,PO,PPPP,A,
     +                PPP,RRR,TR,WW,W1,W2,NH,CL,CM,CH,GPID
      INTEGER         IR,IX,H,VV,N,DD,FF,SN1,TTT,SN2,TD,PO,PPPP,A,
     +                PPP,RRR,TR,WW,W1,W2,NH,CL,CM,CH,GPID
      EQUIVALENCE(WDATA,IR)
C
      PARAMETER (NUMSYM=14,D2RAD=0.017453293)
      CHARACTER*5 IMDAT(NFIELD)
C
C  IPRSNT is an array indicating whether a value for a symbol
C  has been entered.
C
C    IPRSNT -
C     index sym       type          IMDAT          Character #
C     ----- ---  ---------------  --------------   -----------
C       1    CH      high clouds   IMDAT(10)(5:5)         49
C       2    CM    medium clouds   IMDAT(10)(4:4)         48
C       3   TTT      temperature   IMDAT( 3)(2:5)      11-14
C       4   PPP   pressure @ sea   IMDAT( 6)(2:5)      26-29
C       5    VV       visibility   IMDAT( 1)(4:5)       3- 4
C       6    WW     past weather   IMDAT( 9)(4:4)         43
C       7    PP    pressure chg.   IMDAT( 7)(3:5)      32-34
C       8     A        barograph   IMDAT( 7)(2:2)         31
C       9    Td        dew point   IMDAT( 4)(2:5)      16-19
C      10    CL       low clouds   IMDAT(10)(3:3)         47
C      11    Nh        sky cover   IMDAT(10)(2:2)         46
C      12     W   prsent weather   IMDAT( 9)(2:3)      41-42
C      13     h     lowest cloud   IMDAT( 1)(3:3)          2
C      14    RR           precip.  IMDAT( 8)(2:4)      36-38
C
      DIMENSION SYMPOS(2,NUMSYM),IFNTNM(NUMSYM),IPRSNT(NUMSYM)
      CHARACTER*3 CHRS(NUMSYM)
      CHARACTER CHR5*5,CHR3*3,IFMT*8
C
      DIMENSION BRBEXT(4),CHREXT(4),RINTSC(4)
      REAL CTEMP
C
C  Sample character strings to help position labels.
C
      DATA IFNTNM/37,37,21,21,21,37,21,21,21,37,21,21,21,21/
      DATA CHRS/'Q'  ,'E'  ,'XX' ,'XpX','XXX',')'  ,'+Xp',
     +          'X'  ,'XX' ,'?'  ,'XX' ,'W'  ,'X'  ,'XX'  /
C
C  Check error status.
C
      IF (ICFELL('WMSTNM - Uncleared prior error',1) .NE. 0) RETURN
C
C  Color setting.
C
      CALL WMGETI('COL',ICOL)
      ICOLOR = ICOL
      CALL GQTXCI(IER,ITXCOL)
      CALL GSTXCI(ICOL)
      CALL PCGETI('CC',IPCOL)
      CALL PCSETI('CC',ICOL)
C
C  Initialize and decode the data.
C
      DO 210 I=1,NPARMS
        WDATA(I) = 0
  210 CONTINUE
      DO 45 I=1,NUMSYM
        IPRSNT(I) = 0
   45 CONTINUE
      READ(IMDAT(1)(1:1),'(I1)') IR 
      READ(IMDAT(1)(2:2),'(I1)') IX 
      IF (IX.EQ.2 .OR. IX.EQ.5) RETURN
C
      IF (IMDAT(1)(3:3) .EQ. '/') THEN
        IPRSNT(13) = 0
      ELSE
        READ(IMDAT(1)(3:3),'(I1)') H
        IPRSNT(13) = 1
      ENDIF
C
      IF (IMDAT(1)(4:5) .EQ. '//') THEN
        IPRSNT(5) = 0
      ELSE
        READ(IMDAT(1)(4:5),'(I2)') VV
        IPRSNT(5) = 1
      ENDIF
C
      IF (IMDAT(2)(1:1) .EQ. '/') THEN
        N = 0
      ELSE
        READ(IMDAT(2)(1:1),'(I1)') N
      ENDIF
      READ(IMDAT(2)(2:3),'(I2)') DD
      IBFLAG = 1
C
C  Do not plot a wind barb if the wind direction is specified as blanks.
C
      IF (IMDAT(2)(2:3) .EQ. '  ') THEN
        IBFLAG = 0
      ENDIF
      READ(IMDAT(2)(4:5),'(I2)') FF
      DO 200 I=3,NFIELD
        READ(IMDAT(I)(1:1),'(I1)') GPID
        IF (GPID .EQ. 1) THEN
          READ(IMDAT(I)(2:2),'(I1)') SN1
          READ(IMDAT(I)(3:5),'(I3)') TTT
          IF (SN1 .GT. 0) TTT = -TTT
          IPRSNT(3) = 1
        ELSE IF (GPID .EQ. 2) THEN
          READ(IMDAT(I)(2:2),'(I1)') SN2
          READ(IMDAT(I)(3:5),'(I3)') TD
          IPRSNT(9) = 1
          IF (SN2 .GT. 0) TD = -TD
        ELSE IF (GPID .EQ. 3) THEN
          READ(IMDAT(I)(2:5),'(I4)') PO
        ELSE IF (GPID .EQ. 4) THEN
          READ(IMDAT(I)(3:5),'(I3)') PPPP
          IPRSNT(4) = 1
        ELSE IF (GPID .EQ. 5) THEN
          READ(IMDAT(I)(2:2),'(I1)') A
          IPRSNT(8) = 1
          READ(IMDAT(I)(3:5),'(I3)') PPP
          IPRSNT(7) = 1
        ELSE IF (GPID .EQ. 6) THEN
          READ(IMDAT(I)(2:4),'(I3)') RRR
          IPRSNT(14) = 1
          READ(IMDAT(I)(5:5),'(I1)') TR
        ELSE IF (GPID .EQ. 7) THEN
          READ(IMDAT(I)(2:3),'(I2)') WW
          IPRSNT(6) = 1
          READ(IMDAT(I)(4:4),'(I1)') W1
          IPRSNT(12) = 1
          READ(IMDAT(I)(5:5),'(I1)') W2
        ELSE IF (GPID .EQ. 8) THEN
          READ(IMDAT(I)(2:2),'(I1)') NH
          IPRSNT(11) = 1
          READ(IMDAT(I)(3:3),'(I1)') CL
          IPRSNT(10) = 1
          READ(IMDAT(I)(4:4),'(I1)') CM
          IPRSNT(2) = 1
          READ(IMDAT(I)(5:5),'(I1)') CH
          IPRSNT(1) = 1
        ENDIF 
  200 CONTINUE
C
      CALL PCGETI('FN',IFOLD)
      EPS = 0.06*WBSHFT
C
C  Convert X and Y to NDC and work in NDC space.
C
      CALL WMW2NX(1,X,XNDC)
      CALL WMW2NY(1,Y,YNDC)
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
      SIZ = WBLSIZ*WBSHFT
C
C  Draw the wind barb and cloud cover.  Define the barb angle in 
C  degrees and radians (0 degrees is North); define the corresponding
C  trig angle (90 is north) in degrees and radians.
C
      BANGD = REAL(10*DD)
      BANGR = BANGD*D2RAD
      TANGD = REAL(90-10*DD)
      TANGR = TANGD*D2RAD
      UX = COS(TANGR)
      UY = SIN(TANGR)
      CALL WMGETI('WBF',IBO)
      CALL WMSETI('WBF',1)
      IF (IBFLAG .EQ. 1) THEN
        CALL WMBARB(XNDC,YNDC,REAL(FF)*UX,REAL(FF)*UY)
        CALL WMSETI('WBF',IBO)
        IF (FF.EQ.0 .AND. DD.EQ.0) THEN
          CALL NGWSYM('N',N,XNDC,YNDC,WBCLMR*WBSHFT,ICOLOR,0)
        ELSE
          CALL NGWSYM('N',N,XNDC,YNDC,WBBASE*WBSHFT,ICOLOR,0)
        ENDIF
      ENDIF
C
C  Set up initial coordinates for the symbol positions relative to 
C  the the position of the wind barb and its length.
C
C
C  High clouds (CH).
C
      SYMPOS(1,1) = XNDC
      SYMPOS(2,1) = YNDC+0.83*WBSHFT
C
C  Medium clouds (CM).
C
      SYMPOS(1,2) = XNDC
      SYMPOS(2,2) = YNDC+0.47*WBSHFT
C
C  Current temperature (TT).
C
      SYMPOS(1,3) = XNDC-0.5*WBSHFT
      SYMPOS(2,3) = YNDC+0.36*WBSHFT
C
C  Barometric pressure (ppp).
C
      SYMPOS(1,4) = XNDC+0.55*WBSHFT
      SYMPOS(2,4) = YNDC+0.36*WBSHFT
C
C  Visibility (VV).
C
      SYMPOS(1,5) = XNDC-.95*WBSHFT
      SYMPOS(2,5) = YNDC
C
C  Present weather (ww).
C
      SYMPOS(1,6) = XNDC-0.45*WBSHFT
      SYMPOS(2,6) = YNDC
C
C  Pressure change (pp).
C
      SYMPOS(1,7) = XNDC+0.5*WBSHFT
      SYMPOS(2,7) = YNDC
C
C  Pressure tendency (a).
C
      SYMPOS(1,8) = XNDC+1.00*WBSHFT
      SYMPOS(2,8) = YNDC
C
C  Temperature of dewpoint (TD).
C
      SYMPOS(1,9) = XNDC-0.65*WBSHFT
      SYMPOS(2,9) = YNDC-0.42*WBSHFT
C
C  Low clouds (CL).
C
      SYMPOS(1,10) = XNDC-0.17*WBSHFT
      SYMPOS(2,10) = YNDC-0.42*WBSHFT
C
C  Sky cover (NH).
C
      SYMPOS(1,11) = XNDC+0.31*WBSHFT
      SYMPOS(2,11) = YNDC-0.42*WBSHFT
C
C  Past weather (W).
C
      SYMPOS(1,12) = XNDC+0.75*WBSHFT
      SYMPOS(2,12) = YNDC-0.42*WBSHFT
C
C  Cloud height (h).
C
      SYMPOS(1,13) = XNDC-0.12*WBSHFT
      SYMPOS(2,13) = YNDC-0.72*WBSHFT
C
C  Precipitation in last 6 hours (RR).
C
      SYMPOS(1,NUMSYM) = XNDC+0.53*WBSHFT
      SYMPOS(2,NUMSYM) = YNDC-0.72*WBSHFT
C
C  If the wind speed is not calm, revise the symbol positions based 
C  on the wind barb direction and the rectangular area containing the 
C  tick marks on the barb.
C
      IF (FF .EQ. 0) GO TO 220
      CALL WMGETR('WXL',BRBEXT(1))
      CALL WMGETR('WXR',BRBEXT(3))
      CALL WMGETR('WYB',BRBEXT(2))
      CALL WMGETR('WYT',BRBEXT(4))
      IF (BANGD .EQ. 0.) THEN
C
C  Move CH, CM, and TT to the left.
C
        ISYM = 1
        CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +            CHRS(ISYM),SIZ,CHREXT)
        OFFSET = EPS+0.5*(CHREXT(3)-CHREXT(1))
        SYMPOS(1,1) = SYMPOS(1,1)-OFFSET
        SYMPOS(1,2) = SYMPOS(1,2)-OFFSET
        SYMPOS(1,3) = SYMPOS(1,3)-OFFSET
      ELSE IF (BANGD.GT.0. .AND. BANGD.LE.50.) THEN
C
C  If CH is present, find the intersection of its text extent box
C  with that of the extent box of the wind barb, and move symbols
C  CH, CM, and TT left by a suitable amount.
C
        IF (IPRSNT(1) .EQ. 1) THEN
          ISYM = 1
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(1,ISYM) = BRBEXT(1)-EPS-0.5*(CHREXT(3)-CHREXT(1))
          ENDIF
        ENDIF
        ORIG = SYMPOS(2,2)
C
C  If CM is present, find the intersection of its text extent box
C  with that of the extent box of the wind barb, and move symbols
C  CM, and TT left by a suitable amount.
C
        IF (IPRSNT(2) .EQ. 1) THEN
          ISYM = 2
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(1,ISYM) = BRBEXT(1)-EPS-0.5*(CHREXT(3)-CHREXT(1))
            ODIFF = ABS(ORIG-SYMPOS(1,ISYM))
            SYMPOS(1,3) = SYMPOS(1,3)-ODIFF
          ENDIF
        ENDIF
C
C  If the pressure reading runs into the windbarb ticks, move it.
C
        IF (IPRSNT(4) .EQ. 1) THEN
          ISYM = 4
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            OFFSET = (BRBEXT(4)-BRBEXT(2))-(WBFTIC*WBSHFT*SIN(BANGR))
            OFFSET = OFFSET*TAN(BANGR)
            SYMPOS(1,ISYM) = BRBEXT(3)-OFFSET+0.25*WBSHFT+
     +                         0.5*(CHREXT(3)-CHREXT(1))
            OFFSET = 0.0025*BANGD*WBSHFT
            SYMPOS(2,ISYM) = SYMPOS(2,ISYM)-OFFSET
          ENDIF
          IF (BANGD .GE. 25.) THEN
            OFFSET = 0.005*(BANGD-25.)*WBSHFT
            SYMPOS(2,7) = SYMPOS(2,7)-OFFSET
            SYMPOS(2,8) = SYMPOS(2,8)-OFFSET
          ENDIF
        ENDIF
      ELSE IF (BANGD.GT.50. .AND. BANGD.LE.60.) THEN
C
C  Move pressure reading up and pressure change and tendency down.
C
        IF (IPRSNT(4) .EQ. 1) THEN
          ISYM = 4
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(4)+EPS+
     +                         0.5*(CHREXT(4)-CHREXT(2))
          ENDIF
        ENDIF
        IF (IPRSNT(7) .EQ. 1) THEN
          ISYM = 7
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(2)-EPS-
     +                         0.5*(CHREXT(4)-CHREXT(2))
            SYMPOS(2,8) = BRBEXT(2)-EPS-
     +                         0.5*(CHREXT(4)-CHREXT(2))
          ENDIF
        ELSE IF (IPRSNT(8) .EQ. 1) THEN
          ISYM = 8
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(2)-EPS-
     +                         0.5*(CHREXT(4)-CHREXT(2))
          ENDIF
        ENDIF
      ELSE IF (BANGD.GE.60. .AND. BANGD.LT.80.) THEN
C
C  Move pressure reading up and pressure change and tendency down.
C
        IF (IPRSNT(4) .EQ. 1) THEN
          ISYM = 4
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(4)+EPS+
     +                         0.5*(CHREXT(4)-CHREXT(2))
          ENDIF
        ENDIF
        ORIGP = YNDC
        DIFFP = 0.
        IF (IPRSNT(7) .EQ. 1) THEN
          ISYM = 7
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(2)-EPS-
     +                         0.5*(CHREXT(4)-CHREXT(2))
            SYMPOS(2,8) = BRBEXT(2)-EPS-
     +                         0.5*(CHREXT(4)-CHREXT(2))
            DIFFP = ABS(ORIGP-SYMPOS(2,ISYM))  
          ENDIF
        ENDIF
        IF (IPRSNT(8) .EQ. 1) THEN
          ISYM = 8
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(2)-EPS-
     +                         0.5*(CHREXT(4)-CHREXT(2))
            DIFFP = ABS(ORIGP-SYMPOS(2,ISYM))  
          ENDIF
        ENDIF
        IF (DIFFP .GT. 0.125*WBSHFT) THEN
          OFFSET = DIFFP - 0.125*WBSHFT
          SYMPOS(2,9) = SYMPOS(2,9)-OFFSET
          SYMPOS(2,10) = SYMPOS(2,10)-OFFSET
          SYMPOS(2,11) = SYMPOS(2,11)-OFFSET
          SYMPOS(2,12) = SYMPOS(2,12)-OFFSET
          SYMPOS(2,13) = SYMPOS(2,13)-OFFSET
          SYMPOS(2,14) = SYMPOS(2,14)-OFFSET
        ENDIF
      ELSE IF (BANGD.GE.80. .AND. BANGD.LT.100.) THEN
C
C  Move pressure reading up and pressure change and tendency up.
C
        ORIGP = YNDC
        DIFFP = 0.
        IF (IPRSNT(7) .EQ. 1) THEN
          ISYM = 7
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(4)+EPS+
     +                         0.5*(CHREXT(4)-CHREXT(2))
            SYMPOS(2,8) = BRBEXT(4)+EPS+
     +                         0.5*(CHREXT(4)-CHREXT(2))
            DIFFP = ABS(ORIGP-SYMPOS(2,ISYM))  
          ENDIF
        ENDIF
        IF (IPRSNT(8) .EQ. 1) THEN
          ISYM = 8
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(4)+EPS+
     +                         0.5*(CHREXT(4)-CHREXT(2))
            DIFFP = ABS(ORIGP-SYMPOS(2,ISYM))  
          ENDIF
        ENDIF
        IF (DIFFP .GT. 0.) THEN
          SYMPOS(2,4) = SYMPOS(2,4)+DIFFP
        ENDIF
C
C  Move sky cover, past weather, rainfall down.
C
        ORIGP = YNDC-0.42*WBSHFT
        DIFFP = 0.
        IF (IPRSNT(11) .EQ. 1) THEN
          ISYM = 11
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(2)-EPS-
     +                         0.5*(CHREXT(4)-CHREXT(2))
            SYMPOS(2,12) = BRBEXT(2)-EPS-
     +                         0.5*(CHREXT(4)-CHREXT(2))
            DIFFP = ABS(ORIGP-SYMPOS(2,ISYM))  
          ENDIF
        ENDIF
        IF (IPRSNT(12) .EQ. 1) THEN
          ISYM = 12
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(2)-EPS-
     +                         0.5*(CHREXT(4)-CHREXT(2))
            DIFFP = ABS(ORIGP-SYMPOS(2,ISYM))  
          ENDIF
        ENDIF
        IF (DIFFP .GT. 0.) THEN
          SYMPOS(2,14) = SYMPOS(2,14)-DIFFP
        ENDIF
      ELSE IF (BANGD.GE.100. .AND. BANGD.LT.110.) THEN
C
C  Move pressure reading up and pressure change and tendency up.
C  Dew point, low clouds, sky cover, past weather over.
C
        ORIGP = YNDC
        DIFFP = 0.
        IF (IPRSNT(7) .EQ. 1) THEN
          ISYM = 7
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(4)+EPS+
     +                         0.5*(CHREXT(4)-CHREXT(2))
            SYMPOS(2,8) = BRBEXT(4)+EPS+
     +                         0.5*(CHREXT(4)-CHREXT(2))
            DIFFP = ABS(ORIGP-SYMPOS(2,ISYM))  
          ENDIF
        ELSE IF (IPRSNT(8) .EQ. 1) THEN
          ISYM = 8
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(2,ISYM) = BRBEXT(4)+EPS+
     +                         0.5*(CHREXT(4)-CHREXT(2))
            DIFFP = ABS(ORIGP-SYMPOS(2,ISYM))  
          ENDIF
        ENDIF
        IF (DIFFP .GT. 0.) THEN
          SYMPOS(2,4) = SYMPOS(2,4)+DIFFP
        ENDIF
C
C  Dew point, low clouds, sky cover, past weather over.
C
        OFFSET = 0.
        IF (IPRSNT(12) .EQ. 1) THEN
          ISYM = 12
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            OFFSET = ABS(CHREXT(3)-BRBEXT(1))
          ENDIF
        ELSE IF (IPRSNT(11) .EQ. 1) THEN
          ISYM = 11
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            OFFSET = ABS(CHREXT(3)-BRBEXT(1))
          ENDIF
        ENDIF
        SYMPOS(1,9) = SYMPOS(1,9)-OFFSET-EPS
        SYMPOS(1,10) = SYMPOS(1,10)-OFFSET-EPS
        SYMPOS(1,11) = SYMPOS(1,11)-OFFSET-EPS
        SYMPOS(1,12) = SYMPOS(1,12)-OFFSET-EPS
      ELSE IF (BANGD.GE.110. .AND. BANGD.LT.120.) THEN
C
C  Dew point, low clouds, sky cover, past weather over.
C  Cloud height, rainfall over.
C
        OFFSET = 0.
        IF (IPRSNT(12) .EQ. 1) THEN
          ISYM = 12
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            OFFSET = ABS(CHREXT(3)-BRBEXT(1))
          ENDIF
        ELSE IF (IPRSNT(11) .EQ. 1) THEN
          ISYM = 11
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            OFFSET = ABS(CHREXT(3)-BRBEXT(1))
          ENDIF
        ELSE IF (IPRSNT(14) .EQ. 1) THEN
          ISYM = 14
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            OFFSET = ABS(CHREXT(3)-BRBEXT(1))
          ENDIF
        ENDIF
        SYMPOS(1,9) = SYMPOS(1,9)-OFFSET-EPS
        SYMPOS(1,10) = SYMPOS(1,10)-OFFSET-EPS
        SYMPOS(1,11) = SYMPOS(1,11)-OFFSET-EPS
        SYMPOS(1,12) = SYMPOS(1,12)-OFFSET-EPS
        SYMPOS(1,13) = SYMPOS(1,13)-OFFSET
        SYMPOS(1,14) = SYMPOS(1,14)-OFFSET
      ELSE IF (BANGD.GE.120. .AND. BANGD.LT.140.) THEN
C
C  Dew point, low clouds, sky cover to the left.  Past weather to
C  right.  Cloud height, rainfall to left.
C
        OFFSET = 0.
        IF (IPRSNT(11) .EQ. 1) THEN
          ISYM = 11
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            OFFSET = ABS(CHREXT(3)-BRBEXT(1))
          ENDIF
          SYMPOS(1,9) = SYMPOS(1,9)-OFFSET-EPS
          SYMPOS(1,10) = SYMPOS(1,10)-OFFSET-EPS
          SYMPOS(1,11) = SYMPOS(1,11)-OFFSET-EPS
        ENDIF          
        IF (IPRSNT(14) .EQ. 1) THEN
          ISYM = 14
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            OFFSET = ABS(CHREXT(3)-BRBEXT(1))
            SYMPOS(1,13) = SYMPOS(1,13)-OFFSET
            SYMPOS(1,14) = SYMPOS(1,14)-OFFSET
          ENDIF
        ENDIF
        IF (IPRSNT(12) .EQ. 1) THEN
          ISYM = 12
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          CALL WMSECT(BRBEXT,CHREXT,IFLG,RINTSC)
          IF (IFLG .EQ. 1) THEN
            SYMPOS(1,12) = BRBEXT(3)+
     +                       EPS+0.5*(CHREXT(3)-CHREXT(1))
          ENDIF
        ENDIF
      ELSE IF (BANGD.GE.140. .AND. BANGD.LT.190.) THEN
C
        EXPFCT = MAX(0.,0.0133*(170.-BANGD))
        SYMPOS(1,11) = SYMPOS(1,11)+EXPFCT*WBSHFT
        SYMPOS(1,12) = SYMPOS(1,12)+EXPFCT*WBSHFT
        SYMPOS(1,14) = SYMPOS(1,14)+EXPFCT*WBSHFT
        OFFSET = 0.
        IF (IPRSNT(10) .EQ. 1) THEN
          ISYM = 10
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(3) .GT. BRBEXT(1)) THEN
            OFFSET = CHREXT(3)-BRBEXT(1)
            SYMPOS(1,9) = SYMPOS(1,9)-OFFSET-EPS
            SYMPOS(1,10) = SYMPOS(1,10)-OFFSET-EPS
          ENDIF
        ENDIF          
        IF (IPRSNT(13) .EQ. 1) THEN
          ISYM = 13
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(3) .GT. BRBEXT(1)) THEN
            OFFSET = CHREXT(3)-BRBEXT(1)
            SYMPOS(1,13) = SYMPOS(1,13)-OFFSET-EPS
          ENDIF
        ENDIF          
      ELSE IF (BANGD.GE.190. .AND. BANGD.LT.230.) THEN
C
        OFFSET = 0.
        IF (IPRSNT(9) .EQ. 1) THEN
          ISYM = 9
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(3) .GT. BRBEXT(1)) THEN
            OFFSET = CHREXT(3)-BRBEXT(1)
          ENDIF
          SYMPOS(1,9) = SYMPOS(1,9)-OFFSET
        ENDIF          
        IF (IPRSNT(10) .EQ. 1) THEN
          ISYM = 10
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(1) .LT. BRBEXT(3)) THEN
            OFFSET = BRBEXT(3)-CHREXT(1)
            SYMPOS(1,10) = SYMPOS(1,10)+OFFSET
            SYMPOS(1,11) = SYMPOS(1,11)+OFFSET
            SYMPOS(1,12) = SYMPOS(1,12)+OFFSET
          ENDIF
        ENDIF          
        IF (IPRSNT(13) .EQ. 1) THEN
          ISYM = 13
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(1) .LT. BRBEXT(3)) THEN
            OFFSET = BRBEXT(3)-CHREXT(1)
            SYMPOS(1,13) = SYMPOS(1,13)+OFFSET
            SYMPOS(1,14) = SYMPOS(1,14)+OFFSET
          ENDIF
        ENDIF
        IF (IPRSNT(5) .EQ. 1) THEN
          ISYM = 5
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(2) .LT. BRBEXT(4)) THEN
            OFFSET = BRBEXT(4)-CHREXT(2)
            SYMPOS(2,ISYM) = SYMPOS(2,ISYM)+OFFSET+EPS
          ENDIF
        ENDIF          
        IF (IPRSNT(6) .EQ. 1) THEN
          ISYM = 6
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(2) .LT. BRBEXT(4)) THEN
            OFFSET = BRBEXT(4)-CHREXT(2)
            SYMPOS(2,ISYM) = SYMPOS(2,ISYM)+OFFSET+EPS
          ENDIF
        ENDIF          
      ELSE IF (BANGD.GE.230. .AND. BANGD.LT.250.) THEN
        OFFSET = 0.
        IF (IPRSNT(5) .EQ. 1) THEN
          ISYM = 5
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(2) .LT. BRBEXT(4)) THEN
            OFFSET = BRBEXT(4) - CHREXT(2)
            SYMPOS(2,ISYM) = SYMPOS(2,ISYM)+OFFSET+EPS
          ENDIF
        ENDIF          
        IF (IPRSNT(6) .EQ. 1) THEN
          ISYM = 6
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(2) .LT. BRBEXT(4)) THEN
            OFFSET = BRBEXT(4) - CHREXT(2)
            SYMPOS(2,6) = SYMPOS(2,6)+OFFSET+EPS
            SYMPOS(2,3) = SYMPOS(2,3)+OFFSET+EPS
          ENDIF
        ENDIF          
        ADJUST = WBSHFT*(0.38-0.012*(250.-BANGD))
        IF (IPRSNT(9) .EQ. 1) THEN
          ISYM = 9
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(3) .GT. BRBEXT(1)) THEN
            OFFSET = CHREXT(3) - BRBEXT(1) - ADJUST
            SYMPOS(1,9) = SYMPOS(1,9)+OFFSET
            SYMPOS(1,10) = SYMPOS(1,10)+OFFSET
            SYMPOS(1,11) = SYMPOS(1,11)+OFFSET
            SYMPOS(1,12) = SYMPOS(1,12)+OFFSET
          ENDIF
        ENDIF          
      ELSE IF (BANGD.GE.250. .AND. BANGD.LT.265.) THEN
        OFFSET = 0.
        IF (IPRSNT(5) .EQ. 1) THEN
          ISYM = 5
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(2) .LT. BRBEXT(4)) THEN
            OFFSET = BRBEXT(4) - CHREXT(2)
            SYMPOS(2,5) = SYMPOS(2,5)+OFFSET+EPS
          ENDIF
        ENDIF          
        IF (IPRSNT(6) .EQ. 1) THEN
          ISYM = 6
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(2) .LT. BRBEXT(4)) THEN
            OFFSET = BRBEXT(4) - CHREXT(2)
            SYMPOS(2,6) = SYMPOS(2,6)+OFFSET+EPS
            SYMPOS(2,3) = SYMPOS(2,3)+OFFSET+EPS
          ENDIF
        ENDIF          
      ELSE IF (BANGD.GE.265. .AND. BANGD.LT.300.) THEN
        OFFSET = 0.
        ORIGP = YNDC
        DIFFP = 0.
        IF (IPRSNT(3) .EQ. 1) THEN
          ISYM = 3
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(2) .LT. BRBEXT(4)) THEN
            OFFSET = BRBEXT(4) - CHREXT(2)
            SYMPOS(2,3) = SYMPOS(2,3)+OFFSET
          ENDIF
        ENDIF          
        IF (IPRSNT(5) .EQ. 1) THEN
          ISYM = 5
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(4) .GT. BRBEXT(2)) THEN
            OFFSET = CHREXT(4) - BRBEXT(2)
            SYMPOS(2,5) = SYMPOS(2,5)-OFFSET-EPS
            DIFFP = ABS(SYMPOS(2,5)-ORIGP) 
          ENDIF
        ENDIF
        IF (IPRSNT(6) .EQ. 1) THEN
          ISYM = 6
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(4) .GT. BRBEXT(2)) THEN
            OFFSET = CHREXT(4) - BRBEXT(2)
            SYMPOS(2,6) = SYMPOS(2,6)-OFFSET-EPS
            DIFFP = ABS(SYMPOS(2,6)-ORIGP) 
          ENDIF
        ENDIF
        IF (DIFFP .GT. 0.125*WBSHFT) THEN
          OFFSET = DIFFP - 0.125*WBSHFT
          SYMPOS(2,9) = SYMPOS(2,9)-OFFSET-EPS
          SYMPOS(2,10) = SYMPOS(2,10)-OFFSET-EPS
          SYMPOS(2,13) = SYMPOS(2,13)-OFFSET-EPS
          SYMPOS(2,11) = SYMPOS(2,11)-EPS
          SYMPOS(2,12) = SYMPOS(2,12)-EPS
          SYMPOS(2,14) = SYMPOS(2,14)-EPS
        ENDIF
      ELSE IF (BANGD.GE.300. .AND. BANGD.LT.360.) THEN
        OFFSET = 0.
        IF (IPRSNT(1) .EQ. 1) THEN
          ISYM = 1
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(1) .LT. BRBEXT(3)) THEN
            OFFSET = BRBEXT(3) - CHREXT(1)
            SYMPOS(1,ISYM) = SYMPOS(1,ISYM)+OFFSET+EPS
          ENDIF
        ENDIF          
        IF (IPRSNT(2) .EQ. 1) THEN
          ISYM = 2
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(1) .LT. BRBEXT(3)) THEN
            OFFSET = BRBEXT(3) - CHREXT(1)
            SYMPOS(1,ISYM) = SYMPOS(1,ISYM)+OFFSET+EPS
          ENDIF
        ENDIF          
        IF (IPRSNT(4).EQ.1 .AND. BANGD.GE.325.) THEN
          SYMPOS(1,4) = SYMPOS(1,4)+OFFSET+EPS
        ENDIF 
        IF (IPRSNT(3) .EQ. 1) THEN
          ISYM = 3
          CALL WMCEXT(ISYM,IFNTNM(ISYM),SYMPOS(1,ISYM),SYMPOS(2,ISYM),
     +                CHRS(ISYM),SIZ,CHREXT)
          IF (CHREXT(3) .GT. BRBEXT(1)) THEN
            OFFSET = CHREXT(3) - BRBEXT(1)
            SYMPOS(1,ISYM) = SYMPOS(1,ISYM)-OFFSET+0.125*WBSHFT
          ENDIF
        ENDIF
      ENDIF
  220 CONTINUE
C
C  Draw the labels.
C
      CALL PCGETI('TE',ITEOLD)
      CALL PCSETI('TE',1)
      CALL PCGETI('FN',IFOLD)
      CALL PCSETI('FN',4)
C
C   High Clouds.
C
      IF (CH.GE.1 .AND. CH.LE.9) THEN
        IF (IPRSNT(1) .EQ. 1)  THEN
          CALL NGWSYM('CH',CH,SYMPOS(1,1),SYMPOS(2,1),
     +                2.*SIZ,ICOLOR,0)       
        ENDIF
      ENDIF
C
C   Medium clouds.
C
      IF (CM.GE.1 .AND. CM.LE.9) THEN
        IF (IPRSNT(2) .EQ. 1)  THEN
          CALL NGWSYM('CM',CM,SYMPOS(1,2),SYMPOS(2,2),
     +                2.*SIZ,ICOLOR,0)       
        ENDIF
      ENDIF
C
C   Current air temperature.
C
C     The input data is assumed to be expressed in whole degrees C 
C     and tenths.  So, for example, an input value of 211 would 
C     be 21.1 degrees C.  If IUNITS=0 (the default), then use Imperial 
C     units, so an input of 211 would be plotted as 70 degrees F.
C     If IUNITS is non-zero, then the temperature is plotted in
C     metric units as whole degrees C and tenths.  So, 211 would 
C     be plotted as 21.1.
C
      IF (IUNITS .EQ. 0) THEN
        IRTEMP = NINT(0.18*REAL(TTT)+32.)
        CHR5 = ' '
        WRITE(CHR5,'(I5)') IRTEMP
        LL = WMGTLN(CHR5,LEN(CHR5),1)
        IF (IPRSNT(3) .EQ. 1) THEN
          CALL PLCHHQ(SYMPOS(1,3),SYMPOS(2,3),CHR5(LL:LEN(CHR5)),
     +                SIZ,0.,0.)       
        ENDIF
      ELSE
        CTEMP = REAL(TTT)/10.
        CHR5 = ' '
        WRITE(CHR5,'(F5.1)') CTEMP
        LL = WMGTLN(CHR5,LEN(CHR5),1)
        IF (IPRSNT(3) .EQ. 1) THEN
          CALL PLCHHQ(SYMPOS(1,3),SYMPOS(2,3),CHR5(LL:LEN(CHR5)),
     +                SIZ,0.,0.)       
        ENDIF
      ENDIF
C
C   Barometric pressure.
C
      CHR3 = ' '
      WRITE(CHR3,'(I3)') PPPP 
      DO 65 I=1,3
        IF (CHR3(I:I) .EQ. ' ') CHR3(I:I)='0'
   65 CONTINUE
      IF (IPRSNT(4) .EQ. 1) THEN
        CALL PLCHHQ(SYMPOS(1,4),SYMPOS(2,4),CHR3,SIZ,0.,0.)       
      ENDIF
C
C   Horizontal visibility at surface.
C
C   If IVVCOD equals 1, then just plot the raw SYNOP two-character
C   code.  Otherwise, convert to mph (if IUNITS equals 0) or km
C   (if IUNITS equals 1) and plot.
C
      IF (IVVCOD .EQ. 1) THEN
        IF (VV .LE. 50. .OR. VV .GE. 56.) THEN
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),IMDAT(1)(4:5),SIZ,0.,0.)
        ENDIF
        GO TO 150
      ENDIF
C   
C
C   Imperial units if IUNITS .EQ. 0.
C
      IF (IUNITS .EQ. 0) THEN
        IPLFLG = 1
        IFR = 0
        IF (VV .LE. 50) THEN
          IF (VV .EQ. 0) THEN
            IWH = 0
          ELSE
            IWH = VV/16
            IFR = MOD(VV,16)
          ENDIF
        ELSE IF (VV.GE.56 .AND. VV.LE.80) THEN
          IWH = NINT((6. + (VV-56))*0.6213712)
          IFR = 0
        ELSE IF (VV.GT.80 .AND. VV.LT.90) THEN
          IWH = NINT(30. + 5.*(VV-80)*0.6213712)
          IFR = 0
        ELSE IF (VV.EQ.90) THEN
          IWH = 0
        ELSE IF (VV.EQ.91) THEN
          IWH = 0
          IFR = 1
        ELSE IF (VV.EQ.92) THEN
          IWH = 0
          IFR = 2
        ELSE IF (VV.EQ.93) THEN
          IWH = 0
          IFR = 5
        ELSE IF (VV.EQ.94) THEN
          IWH = 0
          IFR = 10
        ELSE IF (VV.EQ.95) THEN
          IWH = 1
          IFR = 4
        ELSE IF (VV.EQ.96) THEN
          IWH = 2
          IFR = 8
        ELSE IF (VV.EQ.97) THEN
          IWH = 6
          IFR = 2
        ELSE IF (VV.EQ.98) THEN
          IWH = 12
          IFR = 8
        ELSE IF (VV.EQ.99) THEN
          IWH = 31
          IFR = 4
        ELSE
          IPLFLG = 0
        ENDIF 
        ASIZ = .67*SIZ
        BOFF = .33*SIZ
C
C  The NOAA chart "Explanation of the Weather Map indicates that 
C  visibilities above 10 should be omitted from the map.
C
        IF (IPRSNT(5).EQ.1 .AND. IWH.LE.10 .AND. IPLFLG.EQ.1) THEN
C
C  If the fraction of a mile is 0, then plot the whole mile number.
C
          IF (IFR .EQ. 0) THEN
            CHR3 = ' '
            WRITE(CHR3,'(I3)') IWH
            LL = WMGTLN(CHR3,LEN(CHR3),1)
            CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),CHR3(LL:3),SIZ,0.,0.)
C
C  Plot the fractional part (the whole part will be plotted after
C  this whole IF block.
C
          ELSE
            CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'/',SIZ,0.,0.)
            IF (IFR .EQ. 1) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '16',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '1',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 2) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '8',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '1',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 3) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '16',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '3',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 4) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '4',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '1',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 5) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '16',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '5',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 6) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '8',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '3',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 7) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '16',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '7',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 8) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '2',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '1',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 9) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '16',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '9',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 10) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '8',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '5',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 11) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '16',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '11',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 12) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '4',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '3',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 13) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '16',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '13',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 14) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '8',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '7',ASIZ,0.,+1.)
            ELSE IF (IFR .EQ. 15) THEN
              CALL PLCHHQ(SYMPOS(1,5)+BOFF,SYMPOS(2,5)-BOFF,
     +                    '16',ASIZ,0.,-1.)
              CALL PLCHHQ(SYMPOS(1,5)-BOFF,SYMPOS(2,5)+BOFF,
     +                    '15',ASIZ,0.,+1.)
            ENDIF
C
C  Plot the whole mile part if required.
C
            IF (IWH .NE. 0) THEN
C
C  Get the left extent of the last character string (which was the
C  numerator of the fraction).
C
              CALL PCGETR('DL',XL)
              XP = SYMPOS(1,5)-2.*BOFF-XL
              CHR3 = ' '
              WRITE(CHR3,'(I3)') IWH
              LL = WMGTLN(CHR3,LEN(CHR3),1)
              CALL PLCHHQ(XP,SYMPOS(2,5),CHR3(LL:3),SIZ,0.,+1.)
            ENDIF
          ENDIF
        ENDIF
      ELSE
C
C  Metric units if IUNITS .NE. 0.
C
        IPLFLG = 1
        IF (VV .LE. 50) THEN
          IF (VV .EQ. 0) THEN
            RWH = 0.
          ELSE
            RWH = 0.1*REAL(VV)
          ENDIF
C
C  Values of VV strictly between 50 and 56 are not used.
C
        ELSE IF (VV.GT.50 .AND. VV.LT.56) THEN
          IPLFLG = 0
        ELSE IF (VV.GE.56 .AND. VV.LE.80) THEN
          RWH = REAL(VV) - 50.
        ELSE IF (VV.GT.80 .AND. VV.LT.90) THEN
          RWH = 30. + 5.*(REAL(VV)-80.)
        ELSE IF (VV.EQ.90) THEN
          RWH = 0.
        ELSE IF (VV.EQ.91) THEN
          RWH = 0.05
        ELSE IF (VV.EQ.92) THEN
          RWH = 0.20
        ELSE IF (VV.EQ.93) THEN
          RWH = 0.50
        ELSE IF (VV.EQ.94) THEN
          RWH = 1.
        ELSE IF (VV.EQ.95) THEN
          RWH = 2.
        ELSE IF (VV.EQ.96) THEN
          RWH = 4.
        ELSE IF (VV.EQ.97) THEN
          RWH = 10.
        ELSE IF (VV.EQ.98) THEN
          RWH = 20.
        ELSE IF (VV.EQ.99) THEN
          RWH = 50.
        ELSE
          IPLFLG = 0
        ENDIF 
        ASIZ = .67*SIZ
        IF (IPRSNT(5).EQ.1 .AND. VV.GE.0 .AND. VV .LE. 50 .AND. 
     +    IPLFLG.EQ.1) THEN
          CHR3 = ' '
          WRITE(CHR3,'(F3.1)') RWH
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),CHR3,SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.GE.56 .AND. VV.LT.89 .AND. 
     +           IPLFLG.EQ.1) THEN
          CHR3 = ' '
          WRITE(CHR3,'(F3.0)') RWH
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),CHR3(1:3),SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.89 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'>70',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.90 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'<.05',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.91 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'.05',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.92 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'0.2',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.93 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'0.5',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.94 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'1.0',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.95 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'2.0',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.96 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'4.0',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.97 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'10.',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.98 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'20.',SIZ,0.,0.)
        ELSE IF (IPRSNT(5).EQ.1 .AND. VV.EQ.99 .AND. IPLFLG.EQ.1) THEN      
          CALL PLCHHQ(SYMPOS(1,5),SYMPOS(2,5),'>50.',SIZ,0.,0.)
        ENDIF
      ENDIF
C
  150 CONTINUE
C     
C  Present weather
C
      IF (IPRSNT(6) .EQ. 1) THEN
        CALL NGWSYM('WW',WW,SYMPOS(1,6),SYMPOS(2,6),2.0*SIZ,ICOLOR,0)
      ENDIF
C
C  Pressure change.
C
      IF (IPRSNT(7) .EQ. 1) THEN
        CHR3 = ' '
        WRITE(CHR3,'(I3)') PPP
        LL = WMGTLN(CHR3,LEN(CHR3),1)
        CHR5 = ' '
        IF (A .LE. 4) THEN
          WRITE(IFMT,502) 4-LL
  502     FORMAT('(''+'',A',I1,')')
          WRITE(CHR5,IFMT) CHR3(LL:3)
        ELSE
          WRITE(IFMT,503) 4-LL
  503     FORMAT('(''-'',A',I1,')')
          WRITE(CHR5,IFMT) CHR3(LL:3)
        ENDIF
        LL = WMGTLN(CHR5,LEN(CHR5),0)
        IF (LL .EQ. 4) THEN
          SYMPOS(1,7) = SYMPOS(1,7)+0.1*WBSHFT
          SYMPOS(1,8) = SYMPOS(1,8)+0.2*WBSHFT
        ENDIF
        CALL PLCHHQ(SYMPOS(1,7),SYMPOS(2,7),CHR5(1:LL),SIZ,0.,0.)       
      ENDIF
C     
C  Pressure tendency.
C
      IF (IPRSNT(8).EQ.1 .AND. A.GE.0 .AND. A.LE.8) THEN
        CALL NGWSYM('a',A,SYMPOS(1,8),SYMPOS(2,8),1.5*SIZ,ICOLOR,0)
      ENDIF
C
C  Dew point.
C
      IF (IUNITS .EQ. 0) THEN
        IRTEMP = NINT(0.18*REAL(TD)+32.)
        CHR5 = ' '
        WRITE(CHR5,'(I5)') IRTEMP
        LL = WMGTLN(CHR5,LEN(CHR5),1)
        IF (IPRSNT(9) .EQ. 1) THEN
          CALL PLCHHQ(SYMPOS(1,9),SYMPOS(2,9),CHR5(LL:LEN(CHR5)),
     +                SIZ,0.,0.)       
        ENDIF
      ELSE
        CTEMP = REAL(TD)/10.
        CHR5 = ' '
        WRITE(CHR5,'(F5.1)') CTEMP
        LL = WMGTLN(CHR5,LEN(CHR5),1)
        IF (IPRSNT(9) .EQ. 1) THEN
          CALL PLCHHQ(SYMPOS(1,9),SYMPOS(2,9),CHR5(LL:LEN(CHR5)),
     +                SIZ,0.,0.)       
        ENDIF
      ENDIF
C
C  Low clouds.
C
      IF (CL.GE.1 .AND. CL.LE.9) THEN
        IF (IPRSNT(10) .EQ. 1)  THEN
          CALL NGWSYM('CL',CL,SYMPOS(1,10),SYMPOS(2,10),
     +                2.*SIZ,ICOLOR,0)       
        ENDIF
      ENDIF
C
C  Cloud cover - low or medium clouds.
C
      WRITE(CHR5,'(I5)') NH
      LL = WMGTLN(CHR5,LEN(CHR5),1)
      IF (IPRSNT(11) .EQ. 1) THEN
        CALL PLCHHQ(SYMPOS(1,11),SYMPOS(2,11),CHR5(LL:LEN(CHR5)),
     +              SIZ,0.,0.)       
      ENDIF
C     
C  Past weather.
C
      IF (IPRSNT(12).EQ.1 .AND. W1.GE.3 .AND. W1.LE.9) THEN
        CALL NGWSYM('W',W1,SYMPOS(1,12),SYMPOS(2,12),1.5*SIZ,ICOLOR,0)
      ENDIF
C
C  Cloud height.
C
      WRITE(CHR5,'(I5)') H
      LL = WMGTLN(CHR5,LEN(CHR5),1)
      IF (IPRSNT(13).EQ.1 .AND. N.GT.0) THEN
        CALL PLCHHQ(SYMPOS(1,13),SYMPOS(2,13),CHR5(LL:LEN(CHR5)),
     +              SIZ,0.,0.)       
      ENDIF
C
C  Precipitation.
C
      IF (IPRSNT(14).EQ.1 .AND. RRR.GT.0) THEN
        IF (RRR .EQ. 990) THEN
          CALL PLCHHQ(SYMPOS(1,14),SYMPOS(2,14),'T',SIZ,0.,0.)       
          GO TO 75
        ENDIF
        IF (RRR .GT. 990) THEN
          INCHES = NINT(10.*REAL(RRR-990)/25.4)
        ELSE
          INCHES = NINT(100.*REAL(RRR)/25.4)
        ENDIF
        CHR5 = ' '
        WRITE(CHR5,'(I5)') INCHES
        LL = WMGTLN(CHR5,LEN(CHR5),1)
        CALL PLCHHQ(SYMPOS(1,14),SYMPOS(2,14),CHR5(LL:LEN(CHR5)),
     +              SIZ,0.,0.)       
      ENDIF
   75 CONTINUE
C
C  Restore original settings.
C
      CALL GSELNT(NTRO)
      CALL GSTXCI(ITXCOL)
      CALL PCSETI('CC',IPCOL)
      CALL PCSETI('FN',IFOLD)
      CALL PCSETI('TE',ITEOLD)
C
      RETURN
      END
