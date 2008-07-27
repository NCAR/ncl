C
C	$Id: gqppar.f,v 1.6 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPPAR(WTYPE,PPAI,NMX,MMX,ERRIND,N,M,PARRAY)
C
C  INQUIRE PREDEFINED PATTERN REPRESENTATION
C
      include 'gkscom.h'
C
      INTEGER WTYPE,PPAI,NMX,MMX,ERRIND,N,M,PARRAY(NMX,MMX)
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
C  Check if index is positive.
C
      IF (PPAI.LT.1) THEN
        ERRIND = 85
        GO TO 100
      ENDIF
C
C  Since no drivers currently support pattern representations, return
C  error indicator 90.  When and if any drivers supports pattern fill,
C  uncomment the remaining code.
C
      ERRIND = 90
      GO TO 100
C
C  Invoke interface.
C
C     FCODE = -120
C     CONT  = 0
C     CALL GZROI(0)
C     IL1   = 4
C     IL2   = 4
C     ID(1) = WTYPE
C     ID(2) = PPAI
C     ID(3) = NMX
C     ID(4) = MMX
C     IWK   = -1
C     CALL GZIQWK(WTYPE,IWK)
C     IF (RERR.NE.0) THEN
C       ERRIND = RERR
C       GOTO 100
C     ENDIF
C     N = ID(5)
C     M = ID(6)
C
C  Bring over the pattern array.
C
C     INDX = (N*M-1)/128
C     IF (INDX.EQ.0) THEN
C       CALL GZFMWK
C       INDX = 0
C       DO 200 J=1,M
C         DO 201 I=1,N
C           INDX = INDX+1
C           PARRAY(I,J) = ID(INDX)
C 201     CONTINUE
C 200   CONTINUE
C     ELSE
C       CALL GZFMWK
C       INDX = 0
C       DO 202 J=1,M
C         DO 203 I=1,N
C           INDX = INDX+1
C           PARRAY(I,J) = ID(INDX)
C           JMD = MOD(INDX,128)
C           IF (JMD.EQ.0.AND.CONT.EQ.1) THEN
C            CALL GZFMWK
C            INDX = 0
C           ENDIF
C 203     CONTINUE
C 202   CONTINUE
C     ENDIF
C     RETURN
C
  100 CONTINUE
      N = -1
      M = -1
      PARRAY(1,1) = -1
C
      RETURN
      END
