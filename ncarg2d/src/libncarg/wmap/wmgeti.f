C
C	$Id: wmgeti.f,v 1.4 1994-10-14 01:23:57 fred Exp $
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
      ELSE IF (CNP(1:3).EQ.'NMS' .OR. CNP(1:3).EQ.'nms' .OR. 
     +    CNP(1:3).EQ.'Nms') THEN
        IVP = NUMSYO
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
      ELSE IF (CNP(1:3).EQ.'RLS' .OR. CNP(1:3).EQ.'rls' .OR. 
     +    CNP(1:3).EQ.'Rls') THEN
        IVP = IRLLSC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'ROS' .OR. CNP(1:3).EQ.'ros' .OR. 
     +    CNP(1:3).EQ.'Ros') THEN
        IVP = IRLOUC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'RBS' .OR. CNP(1:3).EQ.'rbs' .OR. 
     +    CNP(1:3).EQ.'Rbs') THEN
        IVP = IRBLBKC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'DTC' .OR. CNP(1:3).EQ.'dtc' .OR. 
     +    CNP(1:3).EQ.'Dtc') THEN
        IVP = IDOTCO
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'DBC' .OR. CNP(1:3).EQ.'dbc' .OR. 
     +    CNP(1:3).EQ.'Dbc') THEN
        IVP = IDOTBG
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
      ELSE IF (CNP(1:3).EQ.'SC1' .OR. CNP(1:3).EQ.'sc1' .OR. 
     +    CNP(1:3).EQ.'Sc1') THEN
        IVP = ISUNC1
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'SC2' .OR. CNP(1:3).EQ.'sc2' .OR. 
     +    CNP(1:3).EQ.'Sc2') THEN
        IVP = ISUNC2
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'SC3' .OR. CNP(1:3).EQ.'sc3' .OR. 
     +    CNP(1:3).EQ.'Sc3') THEN
        IVP = ISUNC3
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'SC4' .OR. CNP(1:3).EQ.'sc4' .OR. 
     +    CNP(1:3).EQ.'Sc4') THEN
        IVP = ISUNC4
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'CC1' .OR. CNP(1:3).EQ.'cc1' .OR. 
     +    CNP(1:3).EQ.'Cc1') THEN
        IVP = ISUNC1
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'CC2' .OR. CNP(1:3).EQ.'cc2' .OR. 
     +    CNP(1:3).EQ.'Cc2') THEN
        IVP = ICLDC2
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'CC3' .OR. CNP(1:3).EQ.'cc3' .OR. 
     +    CNP(1:3).EQ.'Cc3') THEN
        IVP = ICLDC3
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'LC1' .OR. CNP(1:3).EQ.'lc1' .OR. 
     +    CNP(1:3).EQ.'Lc1') THEN
        IVP = ILTNC1
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'LC2' .OR. CNP(1:3).EQ.'lc2' .OR. 
     +    CNP(1:3).EQ.'Lc2') THEN
        IVP = ILTNC2
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'LC3' .OR. CNP(1:3).EQ.'lc3' .OR. 
     +    CNP(1:3).EQ.'Lc3') THEN
        IVP = ILTNC3
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'WFC' .OR. CNP(1:3).EQ.'wfc' .OR. 
     +    CNP(1:3).EQ.'Wfc') THEN
        IVP = IWARMC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'CFC' .OR. CNP(1:3).EQ.'cfc' .OR. 
     +    CNP(1:3).EQ.'Cfc') THEN
        IVP = ICOLDC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'RC1' .OR. CNP(1:3).EQ.'rc1' .OR. 
     +    CNP(1:3).EQ.'Rc1') THEN
        IVP = IRGLC1
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'RC2' .OR. CNP(1:3).EQ.'rc2' .OR. 
     +    CNP(1:3).EQ.'Rc2') THEN
        IVP = IRGLC2
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'RC3' .OR. CNP(1:3).EQ.'rc3' .OR. 
     +    CNP(1:3).EQ.'Rc3') THEN
        IVP = IRGLC3
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'RC4' .OR. CNP(1:3).EQ.'rc4' .OR. 
     +    CNP(1:3).EQ.'Rc4') THEN
        IVP = IRGLC4
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'RC5' .OR. CNP(1:3).EQ.'rc5' .OR. 
     +    CNP(1:3).EQ.'Rc5') THEN
        IVP = IRGLC5
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'AWC' .OR. CNP(1:3).EQ.'awc' .OR. 
     +    CNP(1:3).EQ.'Awc') THEN
        IVP = IAROWC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'ASC' .OR. CNP(1:3).EQ.'asc' .OR. 
     +    CNP(1:3).EQ.'Asc') THEN
        IVP = IARSHC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'AOC' .OR. CNP(1:3).EQ.'aoc' .OR. 
     +    CNP(1:3).EQ.'Aoc') THEN
        IVP = IAROUC
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'HIS' .OR. CNP(1:3).EQ.'his' .OR. 
     +    CNP(1:3).EQ.'His') THEN
        IVP = IHIGC1
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'HIF' .OR. CNP(1:3).EQ.'hif' .OR. 
     +    CNP(1:3).EQ.'Hif') THEN
        IVP = IHIGC3
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'HIB' .OR. CNP(1:3).EQ.'hib' .OR. 
     +    CNP(1:3).EQ.'Hib') THEN
        IVP = IHIGC2
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'HIC' .OR. CNP(1:3).EQ.'hic' .OR. 
     +    CNP(1:3).EQ.'Hic') THEN
        IVP = IHIGC4
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'LOS' .OR. CNP(1:3).EQ.'los' .OR. 
     +    CNP(1:3).EQ.'Los') THEN
        IVP = ILOWC1
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'LOF' .OR. CNP(1:3).EQ.'lof' .OR. 
     +    CNP(1:3).EQ.'Lof') THEN
        IVP = ILOWC3
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'LOB' .OR. CNP(1:3).EQ.'lob' .OR. 
     +    CNP(1:3).EQ.'Lob') THEN
        IVP = ILOWC2
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
