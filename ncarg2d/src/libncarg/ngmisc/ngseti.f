C
C	$Id: ngseti.f,v 1.1 1994-04-26 18:22:47 fred Exp $
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
C  FU - Full background.
C
      ELSE IF (CNP(1:2).EQ.'FU' .OR. CNP(1:2).EQ.'fu' .OR.
     +         CNP(1:2).EQ.'Fu') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETI - you must set a value for WO before setting FU'
          CALL SETER (CTM(1:60), 4, 1)
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
     +      'NGSETI - you must set a value for WO before setting FU'
          CALL SETER (CTM(1:60), 5, 1)
          GO TO 120
        ENDIF
        IF (IVP .LE. 0) THEN
          CALL SETER ('NGSETI - value for ST must be positive', 6, 1)
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
     +      'NGSETI - you must set a value for WO before setting FU'
          CALL SETER (CTM(1:60), 7, 1)
          GO TO 120
        ENDIF
        IF (IVP .LE. 0) THEN
          CALL SETER ('NGSETI - value for PA must be positive', 8, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        IPTHMX = IVP
        WRITE(IDR(7:16), 510) IPTHMX
        CALL GESC(-1515,1,IDR,1,IDUM,CDUM)
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
          CALL SETER (CTM(1:60), 9, 1)
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
     +      'NGSETI - you must set a value for WO before setting FU'
          CALL SETER (CTM(1:60), 10, 1)
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
     +      'NGSETI - you must set a value for WO before setting FU'
          CALL SETER (CTM(1:60), 11, 1)
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
        ILLX = IVP
        IF (ILLX.LT.-72000 .OR. ILLX.GT.72000)
     +     CALL SETER('NGSETI - LX value out of range', 12, 1)
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
        ILLY = IVP
        IF (ILLY.LT.-72000 .OR. ILLY.GT.72000)
     +     CALL SETER('NGSETI - LY value out of range', 13, 1)
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
        IURX = IVP
        IF (IURX.LT.-72000 .OR. IURX.GT.72000)
     +     CALL SETER('NGSETI - UX value out of range', 14, 1)
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
        IURY = IVP
        IF (IURY.LT.-72000 .OR. IURY.GT.72000)
     +     CALL SETER('NGSETI - UY value out of range', 15, 1)
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
      ELSE
        CTM(1:36) = 'NGSETI - Parameter name not known - '
        CTM(37:38) = CNP(1:2)
        CALL SETER (CTM(1:38), 16, 1)
        GO TO 120
      ENDIF
C
  120 CONTINUE
      RETURN
      END
