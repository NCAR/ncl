C
C	$Id: gqentn.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQENTN(N,ERRIND,OL,NPRIO)
C
C  INQUIRE LIST element OF NORMALIZATION TRANSFORMATION NUMBERS
C
      include 'gkscom.h'
C
      INTEGER N,ERRIND,OL,NPRIO
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that N is in range.
C
      IF (N.LT.0 .OR. N.GT.MNT) THEN
        ERRIND = 2002
        GOTO 100
      ENDIF
      NPRIO = N
      OL    = MNT+1
      RETURN
C
  100 NPRIO = -1
      OL    = -1
      RETURN
      END
