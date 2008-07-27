C
C	$Id: gzroi.f,v 1.4 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
