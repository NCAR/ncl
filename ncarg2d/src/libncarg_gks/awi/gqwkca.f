C
C	$Id: gqwkca.f,v 1.10 2009-04-08 23:18:21 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQWKCA(WTYPE,ERRIND,WKCAT)
C
C  INQUIRE WORKSTATION CATEGORY
C
      include 'gkscom.h'
C
      INTEGER WTYPE,ERRIND,WKCAT
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the workstation type is valid.
C
      CALL GZCKWK(22,-1,IDUM,WTYPE,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Provide the requested information.
C
      IF (WTYPE .EQ. GWSS) THEN
        WKCAT = GWISS
      ELSE IF (WTYPE.EQ.GCGM) THEN
        WKCAT = GMO
      ELSE IF (WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX) THEN
        WKCAT = GMO
      ELSE IF (WTYPE.GE.GCROMIN .AND. WTYPE.LE.GCROMAX) THEN
        WKCAT = GMO
      ELSE IF (WTYPE.EQ.GPDFP .OR. WTYPE.EQ.GPDFL) THEN
        WKCAT = GMO
      ELSE IF (WTYPE.EQ.GXWC .OR. WTYPE.EQ.GXWE) THEN       
        WKCAT = GOUTIN
      ELSE IF (WTYPE .EQ. GDMP) THEN
        WKCAT = GOUTPT
      ELSE IF (WTYPE .EQ. GPIX) THEN
        WKCAT = GOUTIN
      ELSE
        WKCAT = -1
      ENDIF
      RETURN
C
  100 CONTINUE
      WKCAT = -1
      RETURN
      END
