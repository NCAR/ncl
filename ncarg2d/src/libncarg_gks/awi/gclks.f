C
C	$Id: gclks.f,v 1.5 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GCLKS
C
C  CLOSE GKS
C
      INTEGER ECLKS
      PARAMETER (ECLKS=1)
C
      include 'gkscom.h'
C
C  Set GKS state value to GKCL.
C
      IF (OPS.NE.GGKOP) THEN
        ERS = 1
        CALL GERHND(2,ECLKS,ERF)
        ERS = 0
        RETURN
      ELSE
        OPS = GGKCL
      ENDIF
C
C  Set flag to indicate that the current picture is empty.
C
      NOPICT = 0
C
      RETURN
      END
