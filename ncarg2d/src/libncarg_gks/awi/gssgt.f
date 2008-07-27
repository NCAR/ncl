C
C	$Id: gssgt.f,v 1.4 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSSGT(SGNA,M)
C
C  SET SEGMENT TRANSFORMATION
C
      INTEGER ESSGT
      PARAMETER (ESSGT=64)
C
      include 'gkscom.h'
C
      INTEGER SGNA
      REAL    M(2,3)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,ESSGT,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the segment name is valid.
C
      IF (SGNA.LT.0 .OR. SGNA.GT.99) THEN
        ERS = 1
        CALL GERHND(120,ESSGT,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the segment exists.
C
      NXST = 0
      DO 200 I=1,NUMSEG
        IF (SEGS(I) .EQ. SGNA) THEN
          NXST = 1
          ISNDX = I
        ENDIF
  200 CONTINUE
      IF (NXST .EQ. 0) THEN
        ERS = 1
        CALL GERHND(122,ESSGT,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Define the segment transformation matrix for segment SGNA.
C
      SEGT(ISNDX,1,1) = M(1,1)
      SEGT(ISNDX,1,2) = M(1,2)
      SEGT(ISNDX,1,3) = M(1,3)
      SEGT(ISNDX,2,1) = M(2,1)
      SEGT(ISNDX,2,2) = M(2,2)
      SEGT(ISNDX,2,3) = M(2,3)
C
      RETURN
      END
