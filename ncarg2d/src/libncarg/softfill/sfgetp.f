C
C $Id: sfgetp.f,v 1.2 1994-03-17 20:58:38 kennison Exp $
C
      SUBROUTINE SFGETP (IDP)
C
C Dimension the argument array.
C
      DIMENSION IDP(8,8)
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,LCH,LDP(8,8)
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL SFBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFGETP - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the internal dot-pattern array into the user's array.
C
      DO 10001 I=1,8
      DO 10002 J=1,8
      IDP(I,J)=LDP(I,J)
10002 CONTINUE
10001 CONTINUE
C
C Done.
C
      RETURN
C
      END
