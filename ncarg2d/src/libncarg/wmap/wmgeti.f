C
C	$Id: wmgeti.f,v 1.2 1994-09-12 19:07:33 fred Exp $
C
      SUBROUTINE WMGETI (CNP,IVP)
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
C  returned by WMGETI.
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
      IF (ICFELL('WMGETI - Uncleared prior error',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 3) THEN
        CTM(1:36)='WMGETI - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 110
      ENDIF
C
C Get the appropriate parameter value.
C
      IF (CNP(1:3).EQ.'MXS' .OR. CNP(1:3).EQ.'mxs' .OR. 
     +    CNP(1:3).EQ.'Mxs') THEN
        IVP = MAXSYM
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'PAI' .OR. CNP(1:3).EQ.'pai' .OR. 
     +    CNP(1:3).EQ.'Pai') THEN
        IVP = IARNDX
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'SLF' .OR. CNP(1:3).EQ.'slf' .OR. 
     +    CNP(1:3).EQ.'Slf') THEN
        IVP = ISLFLG
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WTY' .OR. CNP(1:3).EQ.'wty' .OR. 
     +    CNP(1:3).EQ.'Wty') THEN
        IVP = IWDTYP
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'COL' .OR. CNP(1:3).EQ.'col' .OR. 
     +    CNP(1:3).EQ.'Col') THEN
        IVP = ICOLOR
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'CBC' .OR. CNP(1:3).EQ.'cbc' .OR. 
     +    CNP(1:3).EQ.'Cbc') THEN
        IVP = IBGCTY
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'RFC' .OR. CNP(1:3).EQ.'rfc' .OR. 
     +    CNP(1:3).EQ.'Rfc') THEN
        IVP = IFGTRG
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'DTC' .OR. CNP(1:3).EQ.'dtc' .OR. 
     +    CNP(1:3).EQ.'Dtc') THEN
        IVP = IDOTCO
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'NBZ' .OR. CNP(1:3).EQ.'nbz' .OR. 
     +    CNP(1:3).EQ.'Nbz') THEN
        IVP = NPTSBZ
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'ALO' .OR. CNP(1:3).EQ.'alo' .OR. 
     +    CNP(1:3).EQ.'Alo') THEN
        IVP = IALOFT
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WBF' .OR. CNP(1:3).EQ.'wbf' .OR. 
     +    CNP(1:3).EQ.'Wbf') THEN
        IVP = IALOFT
        GO TO 110
      ELSE
        CTM(1:36) = 'WMGETI - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 2, 1)
        GO TO 110
      ENDIF
C
  110 CONTINUE
      RETURN
      END
