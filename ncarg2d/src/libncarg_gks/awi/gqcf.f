C
C	$Id: gqcf.f,v 1.6 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQCF(WTYPE,ERRIND,NCOLI,COLA,NPCI)
C
C  INQUIRE COLOUR FACILITIES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,ERRIND,NCOLI,COLA,NPCI
C
C  Check if GKS is in proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check if the workstation type is valid.
C
      CALL GZCKWK(22,-1,IDUM,WTYPE,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check if the workstation category is OUTPUT or OUTIN.
C
      CALL GQWKCA(WTYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
      IF (ERRIND .NE. 0) GO TO 100
      ERRIND = 0
      IF (WTYPE.EQ.GXWC .OR. WTYPE.EQ.GXWE .OR. WTYPE.EQ.GDMP .OR.
     +    WTYPE.EQ.GPIX) THEN
        NCOLI  = 256
        COLA   = 1
        NPCI   = 4
      ELSE
        GO TO 100
      ENDIF
      RETURN
C
  100 CONTINUE
      NCOLI = -1
      COLA  = -1
      NPCI  = -1
C
      RETURN
      END
