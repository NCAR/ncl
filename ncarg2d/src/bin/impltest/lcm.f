C
C	$Id: lcm.f,v 1.4 2008-07-27 00:59:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION LCM(I,J)
      INTEGER REM
      M=IABS(I)
      N=IABS(J)
      LCM=0
      IF (M.LE.0 .OR. N.LE.0) RETURN
  101 REM=MOD(M,N)
      IF (REM.LE.0) GOTO 102
      M=N
      N=REM
      GOTO 101
  102 LCM=IABS(I*J/N)
      RETURN
      END
