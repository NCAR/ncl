C
C	$Id: gzroi.f,v 1.1 1993-01-09 02:04:29 fred Exp $
C
      SUBROUTINE GZROI(IZ)
C
C  Zero out the interface variables depending on IZ:
C
C     IZ = 0  Zero our all variables.
C        = 1  Zero out all variables except IC1 and IC2.
C
      include 'gkscom.h'
C
      IF (IZ .EQ. 0) THEN
        IL1 = 0
        IL2 = 0
        IC1 = 0
        IC2 = 0
        RL1 = 0
        RL2 = 0
        STRL1 = 0
        STRL2 = 0
      ELSE IF (IZ .EQ. 1) THEN
        IL1 = 0
        IL2 = 0
        RL1 = 0
        RL2 = 0
        STRL1 = 0
        STRL2 = 0
      ENDIF
C
      RETURN
      END
