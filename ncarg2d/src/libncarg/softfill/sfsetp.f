C
C	$Id: sfsetp.f,v 1.1.1.1 1992-04-17 22:32:56 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SFSETP (IDP)
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
C Transfer the user's dot-pattern array into the common block.
C
      DO 10001 I=1,8
      DO 10002 J=1,8
      LDP(I,J)=IDP(I,J)
10002 CONTINUE
10001 CONTINUE
C
C Done.
C
      RETURN
C
      END
