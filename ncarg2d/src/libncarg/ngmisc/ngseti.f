C
C	$Id: ngseti.f,v 1.8 1994-09-20 16:49:21 fred Exp $
C
      SUBROUTINE NGSETI (CNP,IVP)
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
      include 'ngcomn.h'
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL NGBLDA
C
C  Input and output data records for ESCAPE function calls.
C
      CHARACTER*80 IDR,CDUM
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('NGSETI - Uncleared prior error',1) .NE. 0) RETURN
C
C  Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36) = 'NGSETI - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 120
      ENDIF
C
C  Do a crude check to see if IVP is within range (primarily to 
C  make sure that a floating point number has not been supplied).
C
      IF (IVP .GT. 10000000) THEN
        CTM(1:42) = 'NGSETI - Parameter value out of range for '
        CTM(43:44) = CNP(1:2)
        CALL SETER (CTM(1:44), 2, 1)
        RETURN
      ENDIF
C
C  Set the appropriate parameter.
C
      IDR = ' '
C
C  WO - Workstation ID
C
      IF (CNP(1:2).EQ.'WO' .OR. CNP(1:2).EQ.'wo' .OR.
     +    CNP(1:2).EQ.'Wo') THEN
C
C  Check to make sure that the referenced workstation is open.
C
        IF (NGCKOP(IVP) .EQ. 0) THEN
          CTM =
     +     'NGSETI - value for WO must reference an open workstation.'       
          CALL SETER (CTM(1:60), 3, 1)
          GO TO 120
        ENDIF
        IWKID = IVP
        GO TO 120
C
C  CL - GKS clipping (0 = off; 1 = on).
C
      ELSE IF (CNP(1:2).EQ.'CL' .OR. CNP(1:2).EQ.'cl' .OR.
     +         CNP(1:2).EQ.'Cl') THEN
        IF (IVP.NE.0 .AND. IVP.NE.1) THEN
          CALL SETER ('NGSETI - value for CL must be zero or one',
     +                 4, 1)
          GO TO 120
        ENDIF
        IGKSCP = IVP
        WRITE(IDR(1:5), 500) IGKSCP
        CALL GESC(-1399,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  ER - Error number limit for GKS errors issued before abort.
C
      ELSE IF (CNP(1:2).EQ.'ER' .OR. CNP(1:2).EQ.'er' .OR.
     +         CNP(1:2).EQ.'Er') THEN
        IERRMX = MAX(0, IVP)
        WRITE(IDR(1:10), 510) IERRMX
        CALL GESC(-1398,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  FU - Full background.
C
      ELSE IF (CNP(1:2).EQ.'FU' .OR. CNP(1:2).EQ.'fu' .OR.
     +         CNP(1:2).EQ.'Fu') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETI - you must set a value for WO before setting FU'
          CALL SETER (CTM(1:60), 5, 1)
          GO TO 120
        ENDIF
        WRITE(IDR, 500) IWKID
  500   FORMAT(I5)
        IF (IVP .NE. 0) THEN
          IFULLB = 1
          IDR(6:7) = ' 1'
        ELSE
          IFULLB = 0
          IDR(6:7) = ' 0'
        ENDIF
        CALL GESC(-1517,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  ST - Stack size limit.
C
      ELSE IF (CNP(1:2).EQ.'ST' .OR. CNP(1:2).EQ.'st' .OR.
     +         CNP(1:2).EQ.'St') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETI - you must set a value for WO before setting ST'
          CALL SETER (CTM(1:60), 7, 1)
          GO TO 120
        ENDIF
        IF (IVP .LE. 0) THEN
          CALL SETER ('NGSETI - value for ST must be positive', 8, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        ISTKMX = IVP
        WRITE(IDR(7:16), 510) ISTKMX
  510   FORMAT(I10)
        CALL GESC(-1514,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  PA - Path size limit.
C
      ELSE IF (CNP(1:2).EQ.'PA' .OR. CNP(1:2).EQ.'pa' .OR.
     +         CNP(1:2).EQ.'Pa') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETI - you must set a value for WO before setting PA'
          CALL SETER (CTM(1:60), 9, 1)
          GO TO 120
        ENDIF
        IF (IVP .LE. 0) THEN
          CALL SETER ('NGSETI - value for PA must be positive', 10, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        IPTHMX = IVP
        WRITE(IDR(7:16), 510) IPTHMX
        CALL GESC(-1515,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  PE - Percentage error allowed in color matching in X output.
C
      ELSE IF (CNP(1:2).EQ.'PE' .OR. CNP(1:2).EQ.'pe' .OR.
     +         CNP(1:2).EQ.'Pe') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETI - you must set a value for WO before setting PE'
          CALL SETER (CTM(1:60), 11, 1)
          GO TO 120
        ENDIF
        IF (IVP.LT.0 .OR. IVP.GT.100) THEN
          CALL SETER ('NGSETI - value for PE must be between 0 and 100',       
     +                    12, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        IPERCX = IVP
        WRITE(IDR(6:10), 500) IPERCX
        CALL GESC(-1400,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  PR - Flag private color map in X output.
C
      ELSE IF (CNP(1:2).EQ.'PR' .OR. CNP(1:2).EQ.'pr' .OR.
     +         CNP(1:2).EQ.'Pr') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETI - you must set a value for WO before setting PR'
          CALL SETER (CTM(1:60), 13, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        IPRIVX = IVP
        WRITE(IDR(7:16), 510) IPRIVX
        CALL GESC(-1401,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  PU - Pause in X driver.
C
      ELSE IF (CNP(1:2).EQ.'PU' .OR. CNP(1:2).EQ.'pu' .OR.
     +         CNP(1:2).EQ.'Pu') THEN
        IXPS = IVP
        IF (IXPS .LT. 0) THEN
          CTM =
     +      'NGSETI - invalid workstation ID for PU parameter.'
          CALL SETER (CTM(1:60), 14, 1)
          GO TO 120
        ENDIF
        WRITE(IDR, 500) IXPS
        CALL GESC(-1396,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  JO - Line join type.
C
      ELSE IF (CNP(1:2).EQ.'JO' .OR. CNP(1:2).EQ.'jo' .OR.
     +         CNP(1:2).EQ.'Jo') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETI - you must set a value for WO before setting JO'
          CALL SETER (CTM(1:60), 15, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        IF (IVP .LE. 0) THEN
          ILJOIN = 0
        ELSE IF (IVP .GE. 2) THEN
          ILJOIN = 2
        ELSE
          ILJOIN = 1
        ENDIF
        WRITE(IDR(7:7), 520) ILJOIN
  520   FORMAT(I1)
        CALL GESC(-1518,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  CA - Line cap type.
C
      ELSE IF (CNP(1:2).EQ.'CA' .OR. CNP(1:2).EQ.'ca' .OR.
     +         CNP(1:2).EQ.'Ca') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETI - you must set a value for WO before setting CA'
          CALL SETER (CTM(1:60), 16, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        IF (IVP .LE. 0) THEN
          ILCAP = 0
        ELSE IF (IVP .GE. 2) THEN
          ILCAP = 2
        ELSE
          ILCAP = 1
        ENDIF
        WRITE(IDR(7:7), 520) ILCAP
        CALL GESC(-1519,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  LX - Lower left X coordinate on PostScript output page.
C
      ELSE IF (CNP(1:2).EQ.'LX' .OR. CNP(1:2).EQ.'lx' .OR.
     +         CNP(1:2).EQ.'Lx') THEN
        IF (IVP.LT.-72000 .OR. IVP.GT.72000) THEN
          CALL SETER('NGSETI - LX value out of range, no action taken',       
     +                17, 1)
          GO TO 120
        ENDIF
        ILLX = IVP
        WRITE(IDR( 2:10), 530) ILLX
        WRITE(IDR(12:20), 530) ILLY
        WRITE(IDR(22:30), 530) IURX
        WRITE(IDR(32:40), 530) IURY
  530   FORMAT(I9)
        CALL GESC(-1521,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  LY - Lower left Y coordinate on PostScript output page.
C
      ELSE IF (CNP(1:2).EQ.'LY' .OR. CNP(1:2).EQ.'ly' .OR.
     +         CNP(1:2).EQ.'Ly') THEN
        IF (IVP.LT.-72000 .OR. IVP.GT.72000) THEN
          CALL SETER('NGSETI - LY value out of range, no action taken',       
     +                18, 1)
          GO TO 120
        ENDIF
        ILLY = IVP
        WRITE(IDR( 2:10), 530) ILLX
        WRITE(IDR(12:20), 530) ILLY
        WRITE(IDR(22:30), 530) IURX
        WRITE(IDR(32:40), 530) IURY
        CALL GESC(-1521,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  UX - Upper right X coordinate on PostScript output page.
C
      ELSE IF (CNP(1:2).EQ.'UX' .OR. CNP(1:2).EQ.'ux' .OR.
     +         CNP(1:2).EQ.'Ux') THEN
        IF (IVP.LT.-72000 .OR. IVP.GT.72000) THEN
          CALL SETER('NGSETI - UX value out of range, no action taken',       
     +                19, 1)
          GO TO 120
        ELSE IF (IVP .LE. ILLX) THEN
          CALL SETER('NGSETI - LX >= UX, no action taken', 20, 1)
          GO TO 120
        ENDIF
        IURX = IVP
        WRITE(IDR( 2:10), 530) ILLX
        WRITE(IDR(12:20), 530) ILLY
        WRITE(IDR(22:30), 530) IURX
        WRITE(IDR(32:40), 530) IURY
        CALL GESC(-1521,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  UY - Upper right X coordinate on PostScript output page.
C
      ELSE IF (CNP(1:2).EQ.'UY' .OR. CNP(1:2).EQ.'uy' .OR.
     +         CNP(1:2).EQ.'Uy') THEN
        IF (IVP.LT.-72000 .OR. IVP.GT.72000) THEN
          CALL SETER('NGSETI - UY value out of range, no action taken',       
     +                21, 1)
          GO TO 120
        ELSE IF (IVP .LE. ILLY) THEN
          CALL SETER('NGSETI - LY >= UY, no action taken', 22, 1)
          GO TO 120
        ENDIF
        IURY = IVP
        WRITE(IDR( 2:10), 530) ILLX
        WRITE(IDR(12:20), 530) ILLY
        WRITE(IDR(22:30), 530) IURX
        WRITE(IDR(32:40), 530) IURY
        CALL GESC(-1521,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  CO - Scale factor for PostScript coordinates.
C
      ELSE IF (CNP(1:2).EQ.'CO' .OR. CNP(1:2).EQ.'co' .OR.
     +         CNP(1:2).EQ.'Co') THEN
        ICOSCL = MAX(IVP,1)
        WRITE(IDR(1:5), 500) ICOSCL
        CALL GESC(-1522,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  CT - Flag to indicate if NGDOTS draws circles or filled dots (0=dots;
C       1=circles).
C
      ELSE IF (CNP(1:2).EQ.'CT' .OR. CNP(1:2).EQ.'ct' .OR.
     +         CNP(1:2).EQ.'Ct') THEN
        ICDFLG = MIN(IVP,1)
        GO TO 120
      ELSE
        CTM(1:36) = 'NGSETI - Parameter name not known - '
        CTM(37:38) = CNP(1:2)
        CALL SETER (CTM(1:38), 23, 1)
        GO TO 120
      ENDIF
C
  120 CONTINUE
      RETURN
      END
