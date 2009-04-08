C
C	$Id: gqlwk.f,v 1.9 2009-04-08 23:18:21 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQLWK(WTYPE,ERRIND,MPLBTE,MPMBTE,MTXBTE,MFABTE,
     +                 MPAI,MCOLI)
C
C  INQUIRE MAXIMUM LENGTH OF WORKSTATION STATE TABLES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,ERRIND,MPLBTE,MPMBTE,MTXBTE,MFABTE,MPAI,MCOLI
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
C  Check if the workstation category is OUTPUT or OUTIN.
C
      CALL GQWKCA(WTYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
      IF (WTYPE.EQ.GXWC .OR. WTYPE.EQ.GXWE .OR. WTYPE.EQ.GDMP .OR.
     +    WTYPE.EQ.GPIX) THEN
        MPLBTE = 0
        MPMBTE = 0
        MTXBTE = 0
        MFABTE = 0
        MPAI   = 0
        MCOLI  = 256
        RETURN
      ENDIF
C
      IF ((WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX) .OR.
     +    (WTYPE.GE.GCROMIN .AND. WTYPE.LE.GCROMAX) .OR.
     +     WTYPE.EQ.GPDFP .OR. WTYPE.EQ.GPDFL) THEN
        DCUNIT = 1
        MPLBTE = 0
        MPMBTE = 0
        MTXBTE = 0
        MFABTE = 0
        MPAI   = 0
        MCOLI  = 256
        RETURN
      ENDIF
C
  100 CONTINUE
      MPLBTE = -1
      MPMBTE = -1
      MTXBTE = -1
      MFABTE = -1
      MPAI   = -1
      MCOLI  = -1
C
      RETURN
      END
