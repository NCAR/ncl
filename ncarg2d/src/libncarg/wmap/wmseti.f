C
C	$Id: wmseti.f,v 1.1 1994-09-09 23:55:35 fred Exp $
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
C Declare the block data routine external to force its loading.
C
      EXTERNAL WMBLDA
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
        IF (IABS(IVP) .GE. 1) THEN
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
        IF (IABS(IVP).GT.2 .OR. IVP.EQ.0) THEN
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
        IF (IABS(IVP).GT.3 .OR. IVP.LT.0) THEN
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
C  COL - specify color index to be used for all objects.
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
C        temperature labels.
C
      ELSE IF (CNP(1:3).EQ.'RFC' .OR. CNP(1:3).EQ.'rfc' .OR.
     +    CNP(1:3).EQ.'Rbc') THEN
        IFGTRG = IVP
        GO TO 120
C
C  DTC - specify color index to be used for the dots marking cities.
C
      ELSE IF (CNP(1:3).EQ.'DTC' .OR. CNP(1:3).EQ.'dtc' .OR.
     +    CNP(1:3).EQ.'dtc') THEN
        IDOTCO = IVP
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
