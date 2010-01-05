C
C	$Id: wmseti.f,v 1.18 2010-01-05 03:52:14 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMSETI (CNP,IVP)
C
      SAVE
C
      CHARACTER*(*) CNP
C
C  This subroutine is called to give an integer value to a specified
C  parameter.
C
C  CNP is the name of the parameter whose value is to be set.
C
C  IVP is an integer variable containing the desired value.
C
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
      IF (ICFELL('WMSETI - Uncleared prior error',1) .NE. 0) RETURN
C
C  Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 3) THEN
        CTM(1:36) = 'WMSETI - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 120
      ENDIF
C
C  Do a crude check to see if IVP is within range (primarily to 
C  make sure that a floating point number has not been supplied).
C
      IF (IVP .GT. 10000000) THEN
        CTM(1:42) = 'WMSETI - Parameter value out of range for '
        CTM(43:45) = CNP(1:3)
        CALL SETER (CTM(1:45), 2, 1)
        GO TO 120
      ENDIF
C
C  MXS - Maximum number of symbols to allow on a front line.
C
      IF (CNP(1:3).EQ.'MXS' .OR. CNP(1:3).EQ.'mxs' .OR.
     +    CNP(1:3).EQ.'Mxs') THEN
        IF (IVP .GT. ISDIM) THEN
          CTM(1:42) = 'WMSETI - Parameter value out of range for '
          CTM(43:45) = CNP(1:3)
          CALL SETER (CTM(1:45), 2, 1)
          GO TO 120
        ENDIF
        MAXSYM = IVP
        GO TO 120
C
C  NMS - force a certain number of symbols on a front line (if > 0).
C
      ELSE IF (CNP(1:3).EQ.'NMS' .OR. CNP(1:3).EQ.'nms' .OR.
     +    CNP(1:3).EQ.'Nms') THEN
        NUMSYO = IVP
        GO TO 120
C
C  PAI - current parameter array index.
C
      ELSE IF (CNP(1:3).EQ.'PAI' .OR. CNP(1:3).EQ.'pai' .OR.
     +    CNP(1:3).EQ.'Pai') THEN
        IF (IVP .GT. ISDIM) THEN
          CTM(1:42) = 'WMSETI - Parameter value out of range for '
          CTM(43:45) = CNP(1:3)
          CALL SETER (CTM(1:45), 2, 1)
          GO TO 120
        ENDIF
        IARNDX = IVP
        GO TO 120
C
C  WTY - flag to indicate how line widths are to be implemented 
C        (0=use GKS linewidths; 1=use internal routines WMDRFL).
C
      ELSE IF (CNP(1:3).EQ.'WTY' .OR. CNP(1:3).EQ.'wty' .OR.
     +    CNP(1:3).EQ.'Wty') THEN
        IF (ABS(IVP) .GE. 1) THEN
          IWDTYP = 1
        ELSE
          IWDTYP = 0
        ENDIF
        GO TO 120
C
C  STY - symbol type (uses PAI to determine position on line).
C
      ELSE IF (CNP(1:3).EQ.'STY' .OR. CNP(1:3).EQ.'sty' .OR.
     +    CNP(1:3).EQ.'Sty') THEN
        IF (ABS(IVP).GT.2 .OR. IVP.EQ.0) THEN
          CTM(1:42) = 'WMSETI - Parameter value out of range for '
          CTM(43:45) = CNP(1:3)
          CALL SETER (CTM(1:45), 2, 1)
          GO TO 120
        ENDIF
        ISTYPE(IARNDX) = IVP
        GO TO 120
C
C  SLF - slope flag (0=use SL1 & SL2; 1=use SL1 only; 2=use SL2 only;
C        3=use neither SL1 or SL2).
C
      ELSE IF (CNP(1:3).EQ.'SLF' .OR. CNP(1:3).EQ.'slf' .OR.
     +    CNP(1:3).EQ.'Slf') THEN
        IF (ABS(IVP).GT.3 .OR. IVP.LT.0) THEN
          ISLFLG = 3
        ELSE
          ISLFLG = IVP
        ENDIF
        GO TO 120
C
C  ALO - flag for surface or aloft.
C
      ELSE IF (CNP(1:3).EQ.'ALO' .OR. CNP(1:3).EQ.'alo' .OR.
     +    CNP(1:3).EQ.'Alo') THEN
        IF (IVP .NE. 0) THEN
          IALOFT = 1
        ELSE
          IALOFT = 0
        ENDIF
        GO TO 120
C
C  COL - specify color index to be used for all objects that require 
C        only a single color setting.
C
      ELSE IF (CNP(1:3).EQ.'COL' .OR. CNP(1:3).EQ.'col' .OR.
     +    CNP(1:3).EQ.'Col') THEN
        ICOLOR = IVP
        GO TO 120
C
C  CBC - specify color index to be used for backgrounds of city and
C        dalily high/low labels.
C
      ELSE IF (CNP(1:3).EQ.'CBC' .OR. CNP(1:3).EQ.'cbc' .OR.
     +    CNP(1:3).EQ.'Cbc') THEN
        IBGCTY = IVP
        GO TO 120
C
C  RFC - specify color index to be used for foreground of regional
C        temperature labels and cities.
C
      ELSE IF (CNP(1:3).EQ.'RFC' .OR. CNP(1:3).EQ.'rfc' .OR.
     +    CNP(1:3).EQ.'Rbc') THEN
        IFGTRG = IVP
        GO TO 120
C
C  RLS - shadow color index for regional temperature labels.
C
      ELSE IF (CNP(1:3).EQ.'RLS' .OR. CNP(1:3).EQ.'rls' .OR.
     +    CNP(1:3).EQ.'Rls') THEN
        IRLLSC = IVP
        GO TO 120
C
C  EZF - EZMAP flag.  Flags if the NCL entries WMBARB, WMDRFT, and
C        WMLABS are being used in conjunction with EZMAP, in which
C        case the input coordinates are assumed to be in degrees
C        latitude and longitude.
C
      ELSE IF (CNP(1:3).EQ.'EZF' .OR. CNP(1:3).EQ.'ezf' .OR.
     +    CNP(1:3).EQ.'Ezf') THEN
        IEZFLG = IVP
        GO TO 120
C
C  ROS - outline color index for regional temperature labels.
C
      ELSE IF (CNP(1:3).EQ.'ROS' .OR. CNP(1:3).EQ.'ros' .OR.
     +    CNP(1:3).EQ.'Ros') THEN
        IRLOUC = IVP
        GO TO 120
C
C  RBS -  color index for background box for regional temperature labels.
C
      ELSE IF (CNP(1:3).EQ.'RBS' .OR. CNP(1:3).EQ.'rbs' .OR.
     +    CNP(1:3).EQ.'Rbs') THEN
        IRLBKC = IVP
        GO TO 120
C
C  DTC - specify color index to be used for the dots marking cities.
C
      ELSE IF (CNP(1:3).EQ.'DTC' .OR. CNP(1:3).EQ.'dtc' .OR.
     +    CNP(1:3).EQ.'dtc') THEN
        IDOTCO = IVP
        GO TO 120
C
C  DBC - specify color background color for dot symbols.
C
      ELSE IF (CNP(1:3).EQ.'DBC' .OR. CNP(1:3).EQ.'dbc' .OR.
     +    CNP(1:3).EQ.'dbc') THEN
        IDOTBG = IVP
        GO TO 120
C
C  NBZ - number of points to use in Bezier curves for warm front
C        symbols.
C
      ELSE IF (CNP(1:3).EQ.'NBZ' .OR. CNP(1:3).EQ.'nbz' .OR.
     +    CNP(1:3).EQ.'Nbz') THEN
        NPTSBZ = MAX(IVP,7)
        GO TO 120
C
C  REV - Reverses the directions that the symbols point.
C
      ELSE IF (CNP(1:3).EQ.'REV' .OR. CNP(1:3).EQ.'rev' .OR.
     +    CNP(1:3).EQ.'Rev') THEN
        DO 10 I=1,MAXSYM
          ISTYPE(I) = -ISTYPE(I)
   10   CONTINUE   
        GO TO 120
C
C  WBF - Flag indicating whether the base of a wind barb should
C        be drawn to allow for the sky cover circle at its base.
C
      ELSE IF (CNP(1:3).EQ.'WBF' .OR. CNP(1:3).EQ.'wbf' .OR.
     +    CNP(1:3).EQ.'Wbf') THEN
        IWBBAS = IVP
        GO TO 120
C
C  WDF - Flag indicating wind barb direction.  The default (=0)
C        means plot the barb shaft away from the wind (which is
C        is not the meteorological convention) and WDF non-zero
C        means plot the barb shaft in the direction of the wind
C        (the meteorological convention). 
C
      ELSE IF (CNP(1:3).EQ.'WDF' .OR. CNP(1:3).EQ.'wdf' .OR.
     +    CNP(1:3).EQ.'Wdf') THEN
        IWBDIR = IVP
        GO TO 120
C
C  SC1 - color index for center of sun symbol.
C
      ELSE IF (CNP(1:3).EQ.'SC1' .OR. CNP(1:3).EQ.'sc1' .OR.
     +    CNP(1:3).EQ.'Sc1') THEN
        ISUNC1 = IVP
        GO TO 120
C
C  SC2 - color index for the points of the sun symbol.
C
      ELSE IF (CNP(1:3).EQ.'SC2' .OR. CNP(1:3).EQ.'sc2' .OR.
     +    CNP(1:3).EQ.'Sc2') THEN
        ISUNC2 = IVP
        GO TO 120
C
C  SC3 - color index for the outline of the sun symbol.
C
      ELSE IF (CNP(1:3).EQ.'SC3' .OR. CNP(1:3).EQ.'sc3' .OR.
     +    CNP(1:3).EQ.'Sc3') THEN
        ISUNC3 = IVP
        GO TO 120
C
C  SC4 - color index for shadow of the sun symbol.
C
      ELSE IF (CNP(1:3).EQ.'SC4' .OR. CNP(1:3).EQ.'sc4' .OR.
     +    CNP(1:3).EQ.'Sc4') THEN
        ISUNC4 = IVP
        GO TO 120
C
C  CC1 - color index for interior of the cloud symbol.
C
      ELSE IF (CNP(1:3).EQ.'CC1' .OR. CNP(1:3).EQ.'cc1' .OR.
     +    CNP(1:3).EQ.'Cc1') THEN
        ICLDC1 = IVP
        GO TO 120
C
C  CC2 - color index for the outline of the cloud symbol.
C
      ELSE IF (CNP(1:3).EQ.'CC2' .OR. CNP(1:3).EQ.'cc2' .OR.
     +    CNP(1:3).EQ.'Cc2') THEN
        ICLDC2 = IVP
        GO TO 120
C
C  CC3 - color index for shadow of the cloud symbol.
C
      ELSE IF (CNP(1:3).EQ.'CC3' .OR. CNP(1:3).EQ.'cc3' .OR.
     +    CNP(1:3).EQ.'Cc3') THEN
        ICLDC3 = IVP
        GO TO 120
C
C  LC1 - color index for the interior of the lightening bolt symbol.
C
      ELSE IF (CNP(1:3).EQ.'LC1' .OR. CNP(1:3).EQ.'lc1' .OR.
     +    CNP(1:3).EQ.'Lc1') THEN
        ILTNC1 = IVP
        GO TO 120
C
C  LC2 - color index for the outline of the lightening bolt symbol.
C
      ELSE IF (CNP(1:3).EQ.'LC2' .OR. CNP(1:3).EQ.'lc2' .OR.
     +    CNP(1:3).EQ.'Lc2') THEN
        ILTNC2 = IVP
        GO TO 120
C
C  LC3 - color index for the shadow of the lightening bolt symbol.
C
      ELSE IF (CNP(1:3).EQ.'LC3' .OR. CNP(1:3).EQ.'lc3' .OR.
     +    CNP(1:3).EQ.'Lc3') THEN
        ILTNC3 = IVP
        GO TO 120
C
C  WFC - color index for warm front symbols.
C
      ELSE IF (CNP(1:3).EQ.'WFC' .OR. CNP(1:3).EQ.'wfc' .OR.
     +    CNP(1:3).EQ.'Wfc') THEN
        IWARMC = IVP
        GO TO 120
C
C  CFC - color index for cold front symbols.
C
      ELSE IF (CNP(1:3).EQ.'CFC' .OR. CNP(1:3).EQ.'cfc' .OR.
     +    CNP(1:3).EQ.'Cfc') THEN
        ICOLDC = IVP
        GO TO 120
C
C  T1C - color index for alternate colors of tropical front.
C
      ELSE IF (CNP(1:3).EQ.'T1C' .OR. CNP(1:3).EQ.'t1c' .OR.
     +    CNP(1:3).EQ.'T1c') THEN
        ITRO1C = IVP
        GO TO 120
C
C  T2C - another color index for alternate colors of tropical front.
C
      ELSE IF (CNP(1:3).EQ.'T2C' .OR. CNP(1:3).EQ.'t2c' .OR.
     +    CNP(1:3).EQ.'T2c') THEN
        ITRO2C = IVP
        GO TO 120
C
C  HIS - shadow color for high symbols.
C
      ELSE IF (CNP(1:3).EQ.'HIS' .OR. CNP(1:3).EQ.'his' .OR.
     +    CNP(1:3).EQ.'His') THEN
        IHIGC1 = IVP
        GO TO 120
C
C  HIF - symbol color for high symbols.
C
      ELSE IF (CNP(1:3).EQ.'HIF' .OR. CNP(1:3).EQ.'hif' .OR.
     +    CNP(1:3).EQ.'Hif') THEN
        IHIGC3 = IVP
        GO TO 120
C
C  HIB - character background color for high symbols.
C
      ELSE IF (CNP(1:3).EQ.'HIB' .OR. CNP(1:3).EQ.'hib' .OR.
     +    CNP(1:3).EQ.'Hib') THEN
        IHIGC2 = IVP
        GO TO 120
C
C  HIC - color of circumscribed circle.
C
      ELSE IF (CNP(1:3).EQ.'HIC' .OR. CNP(1:3).EQ.'hic' .OR.
     +    CNP(1:3).EQ.'Hic') THEN
        IHIGC4 = IVP
        GO TO 120
C
C  LOS - shadow color for low symbols.
C
      ELSE IF (CNP(1:3).EQ.'LOS' .OR. CNP(1:3).EQ.'los' .OR.
     +    CNP(1:3).EQ.'Los') THEN
        ILOWC1 = IVP
        GO TO 120
C
C  LOF - symbol color for low symbols.
C
      ELSE IF (CNP(1:3).EQ.'LOF' .OR. CNP(1:3).EQ.'lof' .OR.
     +    CNP(1:3).EQ.'Lof') THEN
        ILOWC3 = IVP
        GO TO 120
C
C  LOB - character background color for low symbols.
C
      ELSE IF (CNP(1:3).EQ.'LOB' .OR. CNP(1:3).EQ.'lob' .OR.
     +    CNP(1:3).EQ.'Lob') THEN
        ILOWC2 = IVP
        GO TO 120
C
C  LOC - color of circumscribed circle.
C
      ELSE IF (CNP(1:3).EQ.'LOC' .OR. CNP(1:3).EQ.'loc' .OR.
     +    CNP(1:3).EQ.'Loc') THEN
        ILOWC4 = IVP
        GO TO 120
C
C  RC1 - box outline color for regional weather labels.
C
      ELSE IF (CNP(1:3).EQ.'RC1' .OR. CNP(1:3).EQ.'rc1' .OR.
     +    CNP(1:3).EQ.'Rc1') THEN
        IRGLC1 = IVP
        GO TO 120
C
C  RC2 - background color for regional weather labels.
C
      ELSE IF (CNP(1:3).EQ.'RC2' .OR. CNP(1:3).EQ.'rc2' .OR.
     +    CNP(1:3).EQ.'Rc2') THEN
        IRGLC2 = IVP
        GO TO 120
C
C  RC3 - shadow color for regional weather labels.
C
      ELSE IF (CNP(1:3).EQ.'RC3' .OR. CNP(1:3).EQ.'rc3' .OR.
     +    CNP(1:3).EQ.'Rc3') THEN
        IRGLC3 = IVP
        GO TO 120
C
C  RC4 - character color for regional weather labels.
C
      ELSE IF (CNP(1:3).EQ.'RC4' .OR. CNP(1:3).EQ.'rc4' .OR.
     +    CNP(1:3).EQ.'Rc4') THEN
        IRGLC4 = IVP
        GO TO 120
C
C  RC5 - character outline color for regional weather labels.
C
      ELSE IF (CNP(1:3).EQ.'RC5' .OR. CNP(1:3).EQ.'rc5' .OR.
     +    CNP(1:3).EQ.'Rc5') THEN
        IRGLC5 = IVP
        GO TO 120
C
C  AWC - arrow color.
C
      ELSE IF (CNP(1:3).EQ.'AWC' .OR. CNP(1:3).EQ.'awc' .OR.
     +    CNP(1:3).EQ.'Awc') THEN
        IAROWC = IVP
        GO TO 120
C
C  SMF - Flags whether interpolation (=0) or smoothing (=1) is done.
C
      ELSE IF (CNP(1:3).EQ.'SMF' .OR. CNP(1:3).EQ.'smf' .OR.
     +    CNP(1:3).EQ.'Smf') THEN
        ISMOTH = IVP
        GO TO 120
C
C  ASC - arrow shadow color.
C
      ELSE IF (CNP(1:3).EQ.'ASC' .OR. CNP(1:3).EQ.'asc' .OR.
     +    CNP(1:3).EQ.'Asc') THEN
        IARSHC = IVP
        GO TO 120
C
C  AOC - arrow outline color.
C
      ELSE IF (CNP(1:3).EQ.'AOC' .OR. CNP(1:3).EQ.'aoc' .OR.
     +    CNP(1:3).EQ.'Aoc') THEN
        IAROUC = IVP
        GO TO 120
C
C  UNT - flags whether to use imperial units (as described in the
C        NOAA chart), or metric units as per:
C          http://weather.unisys.com/wxp/Appendices/Formats/SYNOP.html
C
      ELSE IF (CNP(1:3).EQ.'UNT' .OR. CNP(1:3).EQ.'unt' .OR.
     +    CNP(1:3).EQ.'Unt') THEN
        IUNITS = IVP
        GO TO 120
C
C  VCC - color index for vectors.
C
      ELSE IF (CNP(1:3).EQ.'VCC' .OR. CNP(1:3).EQ.'vcc' .OR.
     +    CNP(1:3).EQ.'Vcc') THEN
        VCCOLR = IVP
        GO TO 120
C
C  VLF - Foreground color for vector label box.
C
      ELSE IF (CNP(1:3).EQ.'VLF' .OR. CNP(1:3).EQ.'vlf' .OR.
     +         CNP(1:3).EQ.'Vlf') THEN
        VLBLFC = IVP
        GO TO 120
C
C  VLB - Background color for vector label box.
C
      ELSE IF (CNP(1:3).EQ.'VLB' .OR. CNP(1:3).EQ.'vlb' .OR.
     +         CNP(1:3).EQ.'Vlb') THEN
        VLBLBC = IVP
        GO TO 120
C
C  VVC - Flags whether to plot the visibility parameter VV as the
C        raw two-character SYNOP codes.
C
      ELSE IF (CNP(1:3).EQ.'VVC' .OR. CNP(1:3).EQ.'vvc' .OR.
     +         CNP(1:3).EQ.'Vvc') THEN
        IVVCOD = IVP
        GO TO 120
      ELSE
        CTM(1:36) = 'WMSETI - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 23, 1)
        GO TO 120
      ENDIF
C
  120 CONTINUE
      RETURN
      END
