C
C	$Id: nggeti.f,v 1.6 1994-09-20 16:49:17 fred Exp $
C
      SUBROUTINE NGGETI (CNP,IVP)
C
      SAVE
C
      CHARACTER*(*) CNP
C
C  This subroutine is called to retrieve the integer value of a specified
C  parameter.
C
C  CNP is the name of the parameter whose value is to be retrieved.
C
C  IVP is an integer variable in which the desired value is to be
C  returned by NGGETI.
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
C  Check for an uncleared prior error.
C
      IF (ICFELL('NGGETI - Uncleared prior error',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36)='NGGETI - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 110
      ENDIF
C
C Get the appropriate parameter value.
C
      IF (CNP(1:2).EQ.'WO' .OR. CNP(1:2).EQ.'wo' .OR. 
     +    CNP(1:2).EQ.'Wo') THEN
        IVP = IWKID
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'CL' .OR. CNP(1:2).EQ.'cl' .OR. 
     +    CNP(1:2).EQ.'Cl') THEN
        IVP = IGKSCP
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'ER' .OR. CNP(1:2).EQ.'er' .OR. 
     +    CNP(1:2).EQ.'Er') THEN
        IVP = IERRMX
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'FU' .OR. CNP(1:2).EQ.'fu' .OR. 
     +    CNP(1:2).EQ.'Fu') THEN
        IVP = IFULLB
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'PU' .OR. CNP(1:2).EQ.'pu' .OR. 
     +    CNP(1:2).EQ.'Pu') THEN
        IVP = IXPS
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'ST' .OR. CNP(1:2).EQ.'st' .OR. 
     +    CNP(1:2).EQ.'St') THEN
        IVP = ISTKMX
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'PA' .OR. CNP(1:2).EQ.'pa' .OR. 
     +    CNP(1:2).EQ.'Pa') THEN
        IVP = IPTHMX
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'PE' .OR. CNP(1:2).EQ.'pe' .OR. 
     +    CNP(1:2).EQ.'Pe') THEN
        IVP = IPERCX
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'PR' .OR. CNP(1:2).EQ.'pr' .OR. 
     +    CNP(1:2).EQ.'Pr') THEN
        IVP = IPRIVX
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'JO' .OR. CNP(1:2).EQ.'jo' .OR. 
     +    CNP(1:2).EQ.'Jo') THEN
        IVP = ILJOIN
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'CA' .OR. CNP(1:2).EQ.'ca' .OR. 
     +    CNP(1:2).EQ.'Ca') THEN
        IVP = ILCAP
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'LX' .OR. CNP(1:2).EQ.'lx' .OR. 
     +    CNP(1:2).EQ.'Lx') THEN
        IVP = ILLX
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'LY' .OR. CNP(1:2).EQ.'ly' .OR. 
     +    CNP(1:2).EQ.'Ly') THEN
        IVP = ILLY
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'UX' .OR. CNP(1:2).EQ.'ux' .OR. 
     +    CNP(1:2).EQ.'Ux') THEN
        IVP = IURX
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'UY' .OR. CNP(1:2).EQ.'uy' .OR. 
     +    CNP(1:2).EQ.'Uy') THEN
        IVP = IURY
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'CO' .OR. CNP(1:2).EQ.'co' .OR. 
     +    CNP(1:2).EQ.'Co') THEN
        IVP = ICOSCL
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'CT' .OR. CNP(1:2).EQ.'ct' .OR. 
     +    CNP(1:2).EQ.'Ct') THEN
        IVP = ICDFLG
        GO TO 110
      ELSE
        CTM(1:36) = 'NGGETI - Parameter name not known - '
        CTM(37:38) = CNP(1:2)
        CALL SETER (CTM(1:38), 2, 1)
        GO TO 110
      ENDIF
C
  110 CONTINUE
      RETURN
      END
