C
C	$Id: lcm.f,v 1.1.1.1 1992-04-17 22:34:34 ncargd Exp $
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
