C
C	$Id: wmgetr.f,v 1.3 1994-10-14 01:23:59 fred Exp $
C
      SUBROUTINE WMGETR (CNP,RVP)
C
      SAVE
C
      CHARACTER*(*) CNP
C
C  This subroutine is called to retrieve the real value of a specified
C  parameter.
C
C  CNP is the name of the parameter whose value is to be retrieved.
C
C  RVP is a real variable in which the desired value is to be returned
C  by WMGETR.
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
      IF (ICFELL('WMGETR - Uncleared prior error',1) .NE. 0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36)='WMGETR - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 110
      ENDIF
C
C Get the appropriate parameter value.
C
      IF (CNP(1:3).EQ.'SWI' .OR. CNP(1:3).EQ.'swi' .OR. 
     +    CNP(1:3).EQ.'Swi') THEN
        RVP = SYMWID
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'BEG' .OR. CNP(1:3).EQ.'beg' .OR. 
     +    CNP(1:3).EQ.'Beg') THEN
        RVP = BEGDST
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'END' .OR. CNP(1:3).EQ.'end' .OR. 
     +    CNP(1:3).EQ.'End') THEN
        RVP = ENDDST
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'BET' .OR. CNP(1:3).EQ.'bet' .OR. 
     +    CNP(1:3).EQ.'Bet') THEN
        RVP = BETDST
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'LIN' .OR. CNP(1:3).EQ.'lin' .OR. 
     +    CNP(1:3).EQ.'Lin') THEN
        RVP = RLINWD
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'DWD' .OR. CNP(1:3).EQ.'dwd' .OR. 
     +    CNP(1:3).EQ.'Dwd') THEN
        RVP = DLINWD
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'LWD' .OR. CNP(1:3).EQ.'lwd' .OR. 
     +    CNP(1:3).EQ.'Lwd') THEN
        RVP = SLINWD
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'ARC' .OR. CNP(1:3).EQ.'arc' .OR. 
     +    CNP(1:3).EQ.'Arc') THEN
        RVP = CRVLEN
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WHT' .OR. CNP(1:3).EQ.'wht' .OR. 
     +    CNP(1:3).EQ.'Wht') THEN
        RVP = WSIZEW
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'CHT' .OR. CNP(1:3).EQ.'cht' .OR. 
     +    CNP(1:3).EQ.'Cht') THEN
        RVP = WSIZEC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'THT' .OR. CNP(1:3).EQ.'tht' .OR. 
     +    CNP(1:3).EQ.'Tht') THEN
        RVP = WSIZET
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'RHT' .OR. CNP(1:3).EQ.'rht' .OR. 
     +    CNP(1:3).EQ.'Rht') THEN
        RVP = WSIZER
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'SHT' .OR. CNP(1:3).EQ.'sht' .OR. 
     +    CNP(1:3).EQ.'Sht') THEN
        RVP = WSIZES
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'SL1' .OR. CNP(1:3).EQ.'sl1' .OR. 
     +    CNP(1:3).EQ.'Sl1') THEN
        RVP = SLOPE1
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'SL2' .OR. CNP(1:3).EQ.'sl2' .OR. 
     +    CNP(1:3).EQ.'Sl2') THEN
        RVP = SLOPE2
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'CS1' .OR. CNP(1:3).EQ.'cs1' .OR. 
     +    CNP(1:3).EQ.'Cs1') THEN
        RVP = SLOPEL
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'CS2' .OR. CNP(1:3).EQ.'cs2' .OR. 
     +    CNP(1:3).EQ.'Cs2') THEN
        RVP = SLOPER
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'ARS' .OR. CNP(1:3).EQ.'ars' .OR. 
     +    CNP(1:3).EQ.'Ars') THEN
        RVP = ARWSIZ
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'ARL' .OR. CNP(1:3).EQ.'arl' .OR. 
     +    CNP(1:3).EQ.'Arl') THEN
        RVP = ARWLEN
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'ARD' .OR. CNP(1:3).EQ.'ard' .OR. 
     +    CNP(1:3).EQ.'Ard') THEN
        RVP = ARWDIR
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'CMG' .OR. CNP(1:3).EQ.'cmg' .OR. 
     +    CNP(1:3).EQ.'Cmg') THEN
        RVP = CTYMRG
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'RMG' .OR. CNP(1:3).EQ.'rmg' .OR. 
     +    CNP(1:3).EQ.'Rmg') THEN
        RVP = TMPMRG
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WBS' .OR. CNP(1:3).EQ.'wbs' .OR. 
     +    CNP(1:3).EQ.'Wbs') THEN
        RVP = WBSHFT
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WBT' .OR. CNP(1:3).EQ.'wbt' .OR. 
     +    CNP(1:3).EQ.'Wbt') THEN
        RVP = WBFTIC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WBD' .OR. CNP(1:3).EQ.'wbd' .OR. 
     +    CNP(1:3).EQ.'Wbd') THEN
        RVP = WBDIST
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WBR' .OR. CNP(1:3).EQ.'wbr' .OR. 
     +    CNP(1:3).EQ.'Wbr') THEN
        RVP = WBCLMR
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WBA' .OR. CNP(1:3).EQ.'wba' .OR. 
     +    CNP(1:3).EQ.'Wba') THEN
        RVP = WBBANG
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WBC' .OR. CNP(1:3).EQ.'wbc' .OR. 
     +    CNP(1:3).EQ.'Wbc') THEN
        RVP = WBBASE
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WXL' .OR. CNP(1:3).EQ.'wxl' .OR. 
     +    CNP(1:3).EQ.'Wxl') THEN
        RVP = WBXL
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WXR' .OR. CNP(1:3).EQ.'wxr' .OR. 
     +    CNP(1:3).EQ.'Wxr') THEN
        RVP = WBXR
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WYB' .OR. CNP(1:3).EQ.'wyb' .OR. 
     +    CNP(1:3).EQ.'Wyb') THEN
        RVP = WBYB
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WYT' .OR. CNP(1:3).EQ.'wyt' .OR. 
     +    CNP(1:3).EQ.'Wyt') THEN
        RVP = WBYT
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'DTS' .OR. CNP(1:3).EQ.'dts' .OR. 
     +    CNP(1:3).EQ.'Dts') THEN
        RVP = CDOTSZ
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WBL' .OR. CNP(1:3).EQ.'wbl' .OR. 
     +    CNP(1:3).EQ.'Wbl') THEN
        RVP = WBLSIZ
        GO TO 110
      ELSE
        CTM(1:36) = 'WMGETR - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 2, 1)
        GO TO 110
      ENDIF
C
  110 CONTINUE
      RETURN
      END
