C
C	$Id: gqwkc.f,v 1.10 2009-04-08 23:18:21 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQWKC(WKID,ERRIND,CONID,WTYPE)
C
C  INQUIRE WORKSTATION CONNECTION AND TYPE.
C
      include 'gkscom.h'
C
      INTEGER WKID,ERRIND,CONID,WTYPE
C
C  Check if GKS is in proper state.
C
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the workstation identifier is valid.
C
      CALL GZCKWK(20,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Determine type.
C
      DO 200 I=1,NOPWK
        IF (SOPWK(I) .EQ. WKID) THEN
          WTYPE = SWKTP(I)
          GO TO 10
        ENDIF
  200 CONTINUE
      GO TO 100
   10 CONTINUE
      IF (WTYPE .EQ. GWSS) THEN
        CONID = WCONID
C
C  Connection ID is not used for C drivers.
C
      ELSE IF (WTYPE.EQ.GXWC  .OR. WTYPE.EQ.GDMP  .OR.
     +         WTYPE.EQ.GXWE  .OR. WTYPE.EQ.GPIX  .OR.
     +        (WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX) .OR.
     +        (WTYPE.EQ.GPDFP .OR. WTYPE.EQ.GPDFL) .OR.
     +        (WTYPE.GE.GCROMIN .AND. WTYPE.LE.GCROMAX)) THEN
        CONID = -1
      ELSE 
C
C  Invoke interface.
C
        FCODE = -226
        CONT  = 0
        CALL GZROI(0)
        IL1   = 1
        IL2   = 1
        ID(1) = WKID
        CALL GZIQWK(WTYPE,WKID)
        IF (RERR.NE.0) THEN
          ERRIND = RERR
          GOTO 100
        ENDIF
        CONID = ID(2)
        WTYPE = ID(3)
      ENDIF
      RETURN
C
  100 CONTINUE
      CONID = -1
      WTYPE = -1
      RETURN
      END
