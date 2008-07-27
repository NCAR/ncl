C
C	$Id: gqpxa.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPXA(WKID,PX,PY,DIMX,DIMY,NCS,NRS,DX,DY,
     +                 ERRIND,INVVAL,COLIA)
C
C  INQUIRE PIXEL ARRAY
C
      include 'gkscom.h'
C
      INTEGER WKID,DX,DY,DIMX,ERRIND,INVVAL,COLIA(DIMX,DY)
      REAL    PX,PY
C
C  Check if GKS is in the proper state.
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
C  Check for invalid workstation categories.
C
      CALL GQWKC(WKID,ERRIND,ICONID,ITYPE)
      IF (ERRIND .NE. 0) GO TO 100
      CALL GQWKCA(ITYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
C  Check if array is large enough.
C
      IF (DX.GT.DIMX .OR. DY.GT.DIMY) THEN
        ERRIND = 2001
        GOTO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -291
      CONT  = 0
      CALL GZROI(0)
      IL1   = 7
      IL2   = 7
      RL1   = 1
      RL2   = 1
      ID(1) = WKID
      ID(2) = DIMX
      ID(3) = DIMY
      ID(4) = NCS
      ID(5) = NRS
      ID(6) = DX
      ID(7) = DY
      CALL GZW2NX(1,PX,PXD)
      CALL GZW2NY(1,PY,PYD)
      RX(1) = PXD
      RY(1) = PYD
      CALL GZIQWK(ITYPE,WKID)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      INVVAL = ID(8)
C
C  Bring over the color index array.
C
      INDX = (DX*DY-1)/128
      IF (INDX .EQ. 0) THEN
        CALL GZFMWK
        INDX = 0
        DO 200 J=1,DY
          DO 201 I=1,DX
            INDX = INDX+1
            COLIA(I,J) = ID(INDX)
  201     CONTINUE
  200   CONTINUE
      ELSE
        CALL GZFMWK
        INDX = 0
        DO 202 J=1,DY
          DO 203 I=1,DX
            INDX = INDX+1
            COLIA(I,J) = ID(INDX)
            JMD = MOD(INDX,128)
            IF (JMD.EQ.0.AND.CONT.EQ.1) THEN
              CALL GZFMWK
              INDX = 0
            ENDIF
  203     CONTINUE
  202   CONTINUE
      ENDIF
C
      RETURN
C
  100 CONTINUE
      INVVAL = -1
      COLIA(1,1) = -1
C
      RETURN
      END
