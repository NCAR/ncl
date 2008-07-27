C
C	$Id: gqtxf.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQTXF(WTYPE,N,ERRIND,NFPP,FONT,PREC,NCHH,MINCHH,
     +                 MAXCHH,NCHX,MINCHX,MAXCHX,NPTXI)
C
C  INQUIRE TEXT FACILITIES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,N,ERRIND,NFPP,FONT,PREC,NCHH,NCHX,NPTXI
      REAL    MINCHH,MAXCHH,MINCHX,MAXCHX
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
C  Invoke interface.
C
      FCODE = -124
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = N
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      NFPP   = ID(3)
      FONT   = ID(4)
      PREC   = ID(5)
      NCHH   = ID(6)
      NCHX   = ID(7)
      NPTXI  = ID(8)
      MINCHH = RX(1)
      MAXCHH = RX(2)
      MINCHX = RX(3)
      MAXCHX = RX(4)
      RETURN
C
  100 CONTINUE
      NFPP   = -1
      FONT   = -1
      PREC   = -1
      NCHH   = -1
      NCHX   = -1
      NPTXI  = -1
      MINCHH = 1.E20
      MAXCHH = -1.E20
      MINCHX = 1.E20
      MAXCHX = -1.E20
      RETURN
      END
