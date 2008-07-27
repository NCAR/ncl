C
C	$Id: wmsetr.f,v 1.11 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMSETR (CNP,RVP)
C
      SAVE
C
      CHARACTER*(*) CNP
C
C  This subroutine is called to give a real value to a specified
C  parameter.
C
C  CNP is the name of the parameter whose value is to be set.
C
C  RVP is a real variable containing the desired value.
C
C  Declare a local character variable in which to form an error message.       
C
      CHARACTER*80 CTM
C
      include 'wmcomn.h'
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL WMBLDA
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('WMSETR - Uncleared prior error',1) .NE. 0) RETURN
C
C  Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36) = 'WMSETR - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 120
      ENDIF
C
C  SWI - width of a front symbol, in NDC coordinates.
C
      IF (CNP(1:3).EQ.'SWI' .OR. CNP(1:3).EQ.'swi' .OR.
     +         CNP(1:3).EQ.'Swi') THEN
        SYMWID = RVP
        GO TO 120
C
C  BLW - line width scale factor for drawing wind barbs.
C
      ELSE IF (CNP(1:3).EQ.'BLW' .OR. CNP(1:3).EQ.'blw' .OR.
     +         CNP(1:3).EQ.'Blw') THEN
        BRBLWD = RVP
        GO TO 120
C
C  LIN - specify line widths as a fraction of the screen height.
C
      ELSE IF (CNP(1:3).EQ.'LIN' .OR. CNP(1:3).EQ.'lin' .OR.
     +         CNP(1:3).EQ.'Lin') THEN
        RLINWD = MAX(RVP,0.05)
        GO TO 120
C
C  DWD - specify line widths as a fraction of the screen height for
C        fronts with dashed lines.
C
      ELSE IF (CNP(1:3).EQ.'DWD' .OR. CNP(1:3).EQ.'dwd' .OR.
     +         CNP(1:3).EQ.'Dwd') THEN
        DLINWD = MAX(RVP,0.05)
        GO TO 120
C
C  LWD - linewidth to use when using the internal routine WMDRFL
C        to draw the lines.
C
      ELSE IF (CNP(1:3).EQ.'LWD' .OR. CNP(1:3).EQ.'lwd' .OR.
     +         CNP(1:3).EQ.'Lwd') THEN
        SLINWD = MAX(RVP,0.0001)
        GO TO 120
C
C  SIG - Tension factor for tension spline routines.
C
      ELSE IF (CNP(1:3).EQ.'SIG' .OR. CNP(1:3).EQ.'sig' .OR.
     +         CNP(1:3).EQ.'Sig') THEN
        TNSION = RVP
        GO TO 120
C
C  SMT - Smoothing parameter for the smoothing spline routines.
C
      ELSE IF (CNP(1:3).EQ.'SMT' .OR. CNP(1:3).EQ.'smt' .OR.
     +         CNP(1:3).EQ.'Smt') THEN
        RSMOTH = RVP
        GO TO 120
C
C  OER - Observational weights for data for smoothing spline routines.
C
      ELSE IF (CNP(1:3).EQ.'OER' .OR. CNP(1:3).EQ.'oer' .OR.
     +         CNP(1:3).EQ.'Oer') THEN
        OBSERR = RVP
        GO TO 120
C
C  SHT - Height of symbols plotted with WMLABS.
C
      ELSE IF (CNP(1:3).EQ.'SHT' .OR. CNP(1:3).EQ.'sht' .OR.
     +         CNP(1:3).EQ.'Sht') THEN
        WSIZES = RVP
        GO TO 120
C
C  THT - Height of the regional temperature labels plotted with WMLABT.
C
      ELSE IF (CNP(1:3).EQ.'THT' .OR. CNP(1:3).EQ.'tht' .OR.
     +         CNP(1:3).EQ.'Tht') THEN
        WSIZET = RVP
        GO TO 120
C
C  CHT - Height of the labels for city names and daily high/low temps.
C        plotted with WMLABC.
C
      ELSE IF (CNP(1:3).EQ.'CHT' .OR. CNP(1:3).EQ.'cht' .OR.
     +         CNP(1:3).EQ.'Cht') THEN
        WSIZEC = RVP
        GO TO 120
C
C  WHT - Height of the boxed weather type labels like "BREEZY", "NICE", 
C        etc. plotted with WMLABW.
C
      ELSE IF (CNP(1:3).EQ.'WHT' .OR. CNP(1:3).EQ.'wht' .OR.
     +         CNP(1:3).EQ.'Wht') THEN
        WSIZEW = RVP
        GO TO 120
C
C  RHT - Height of the little symbols used to indicate regional weather
C        patterns, like showers, snow, ice.
C
      ELSE IF (CNP(1:3).EQ.'RHT' .OR. CNP(1:3).EQ.'rht' .OR.
     +         CNP(1:3).EQ.'Rht') THEN
        WSIZER = RVP
        GO TO 120
C
C  BEG - Space to leave at the beginning of a front line before the
C        first symbol is drawn.
C
      ELSE IF (CNP(1:3).EQ.'BEG' .OR. CNP(1:3).EQ.'beg' .OR.
     +         CNP(1:3).EQ.'Beg') THEN
        BEGDST = RVP
        GO TO 120
C
C  END - Space to leave at the end of a front line after the final
C        symbol has been drawn.
C
      ELSE IF (CNP(1:3).EQ.'END' .OR. CNP(1:3).EQ.'end' .OR.
     +         CNP(1:3).EQ.'End') THEN
        ENDDST = RVP
        GO TO 120
C
C  BET - Space to leave along a front line between symbols.
C
      ELSE IF (CNP(1:3).EQ.'BET' .OR. CNP(1:3).EQ.'bet' .OR.
     +         CNP(1:3).EQ.'Bet') THEN
        BETDST = RVP
        GO TO 120
C
C  ARS - Size of arrows as a fraction of the maximum screen height.
C
      ELSE IF (CNP(1:3).EQ.'ARS' .OR. CNP(1:3).EQ.'ars' .OR.
     +         CNP(1:3).EQ.'Ars') THEN
        ARWSIZ = MAX(0.,RVP)
        GO TO 120
C
C  ARD - Direction of arrows, in degrees.
C
      ELSE IF (CNP(1:3).EQ.'ARD' .OR. CNP(1:3).EQ.'ard' .OR.
     +         CNP(1:3).EQ.'Ard') THEN
        ARWDIR = RVP
        GO TO 120
C
C  ARL - scale factor for the length of an arrow's tail, independent
C        of the arrow's size.
C
      ELSE IF (CNP(1:3).EQ.'ARL' .OR. CNP(1:3).EQ.'arl' .OR.
     +         CNP(1:3).EQ.'Arl') THEN
        ARWLEN = MAX(0.,RVP)
        GO TO 120
C
C  DTS - Size of dots used to mark city locations.
C
      ELSE IF (CNP(1:3).EQ.'DTS' .OR. CNP(1:3).EQ.'dts' .OR.
     +         CNP(1:3).EQ.'Dts') THEN
        CDOTSZ = MAX(0.,RVP)
        GO TO 120
C
C  CMG - Size of margins for the city and daily high/low labels.
C
      ELSE IF (CNP(1:3).EQ.'CMG' .OR. CNP(1:3).EQ.'cmg' .OR.
     +         CNP(1:3).EQ.'Cmg') THEN
        CTYMRG = MAX(0.,RVP)
        GO TO 120
C
C  RMG - Size of margins for the regional temperature labels.
C
      ELSE IF (CNP(1:3).EQ.'RMG' .OR. CNP(1:3).EQ.'rmg' .OR.
     +         CNP(1:3).EQ.'Rmg') THEN
        TMPMRG = MAX(0.,RVP)
        GO TO 120
C
C  SL1 - Slope for beginning of spline curve, used in conjunction
C        with SLF.
C
      ELSE IF (CNP(1:3).EQ.'SL1' .OR. CNP(1:3).EQ.'sl1' .OR.
     +         CNP(1:3).EQ.'Sl1') THEN
        SLOPE1 = RVP
        GO TO 120
C
C  SL2 - Slope for end of spline curve, used in conjunction
C        with SLF.
C
      ELSE IF (CNP(1:3).EQ.'SL2' .OR. CNP(1:3).EQ.'sl2' .OR.
     +         CNP(1:3).EQ.'Sl2') THEN
        SLOPE2 = RVP
        GO TO 120
C
C  WBA - Angle wind barb tics make with the wind barb shaft, in degrees.
C
      ELSE IF (CNP(1:3).EQ.'WBA' .OR. CNP(1:3).EQ.'wba' .OR.
     +    CNP(1:3).EQ.'Wba') THEN
        WBBANG = RVP
        GO TO 120
C
C  WBS - Length of wind barb shafts as a fraction of the maximum
C        screen width.
C
      ELSE IF (CNP(1:3).EQ.'WBS' .OR. CNP(1:3).EQ.'wbs' .OR.
     +         CNP(1:3).EQ.'Wbs') THEN
        WBSHFT = RVP
        GO TO 120
C
C  WBT - Length of wind barb full tic as a percentage of its shaft
C        length.
C
      ELSE IF (CNP(1:3).EQ.'WBT' .OR. CNP(1:3).EQ.'wbt' .OR.
     +         CNP(1:3).EQ.'Wbt') THEN
        WBFTIC = RVP
        GO TO 120
C
C  WBD - Distance between tics along a windbarb shaft.
C
      ELSE IF (CNP(1:3).EQ.'WBD' .OR. CNP(1:3).EQ.'wbd' .OR.
     +         CNP(1:3).EQ.'Wbd') THEN
        WBDIST = RVP
        GO TO 120
C
C  WBR - Radius of the larger circle drawn for calm, as a percentage
C        of the windbarb shaft length.
C
      ELSE IF (CNP(1:3).EQ.'WBR' .OR. CNP(1:3).EQ.'wbr' .OR.
     +         CNP(1:3).EQ.'Wbr') THEN
        WBCLMR = RVP
        GO TO 120
C
C  WBC - Diameter of sky cover circle at base of wind barb, expressed
C        as a percentage of the shaft length.
C
      ELSE IF (CNP(1:3).EQ.'WBC' .OR. CNP(1:3).EQ.'wbc' .OR.
     +         CNP(1:3).EQ.'Wbc') THEN
        WBBASE = ABS(RVP)
        GO TO 120
C
C  WBL - Size of the text labels in the station model display as a
C        percentage of the shaft length.
C
      ELSE IF (CNP(1:3).EQ.'WBL' .OR. CNP(1:3).EQ.'wbl' .OR.
     +         CNP(1:3).EQ.'Wbl') THEN
        WBLSIZ = ABS(RVP)
        GO TO 120
C
C  VRS - Size of the wind speed vector that will be scaled
C        to the NDC length specified by VRN.  All vectors
C        will be scaled in keeping with the settings of
C        VRS and VRN.
C
      ELSE IF (CNP(1:3).EQ.'VRS' .OR. CNP(1:3).EQ.'vrs' .OR.
     +         CNP(1:3).EQ.'Vrs') THEN
        VCUREF = ABS(RVP)
        GO TO 120
C
C  VRN - Size, in NDC coordinates, that a vector of length
C        specified by VRS is to scale to.  All vectors
C        will be scaled in keeping with the settings of
C        VRS and VRN.
C
      ELSE IF (CNP(1:3).EQ.'VRN' .OR. CNP(1:3).EQ.'vrn' .OR.
     +         CNP(1:3).EQ.'Vrn') THEN
        VCNREF = ABS(RVP)
        GO TO 120
C
C  VCH - Scale factor for the arrow heads of vectors.
C
      ELSE IF (CNP(1:3).EQ.'VCH' .OR. CNP(1:3).EQ.'vch' .OR.
     +         CNP(1:3).EQ.'Vch') THEN
        VCHSIZ = RVP
        GO TO 120
C
C  VCD - direction of vector (in degrees).
C
      ELSE IF (CNP(1:3).EQ.'VCD' .OR. CNP(1:3).EQ.'vcd' .OR.
     +         CNP(1:3).EQ.'Vcd') THEN
        VCWDIR = RVP
        GO TO 120
C
C  VCW - vector linewidth scale factor.
C
      ELSE IF (CNP(1:3).EQ.'VCW' .OR. CNP(1:3).EQ.'vcw' .OR.
     +         CNP(1:3).EQ.'Vcw') THEN
        VCLWID = ABS(RVP)
        GO TO 120
C
C  VCS - vector size.
C
      ELSE IF (CNP(1:3).EQ.'VCS' .OR. CNP(1:3).EQ.'vcs' .OR.
     +         CNP(1:3).EQ.'Vcs') THEN
        VCSIZE = ABS(RVP)
        GO TO 120
C
C  VVA - vector arrow head angle from tail.
C
      ELSE IF (CNP(1:3).EQ.'VVA' .OR. CNP(1:3).EQ.'vva' .OR.
     +         CNP(1:3).EQ.'Vva') THEN
        VHDANG = ABS(RVP)
        GO TO 120
C
      ELSE
        CTM(1:36) = 'WMSETR - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 8, 1)
        GO TO 120
      ENDIF
C
  120 CONTINUE
      RETURN
      END
