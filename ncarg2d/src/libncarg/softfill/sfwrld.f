C
C	$Id: sfwrld.f,v 1.1.1.1 1992-04-17 22:32:54 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND)
C
C Declare the dimensions of argument arrays.
C
      DIMENSION XRA(NRA),YRA(NRA),DST(NST),IND(NND)
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,LCH,LDP(8,8)
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL SFBLDA
C
C Convert the data to the proper units.
C
      DO 10001 I=1,NRA
      XRA(I)=CUFX(XRA(I))
      YRA(I)=CUFY(YRA(I))
10001 CONTINUE
C
C Call the routine SFNORM to finish the job.
C
      CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
C
C Done.
C
      RETURN
C
      END
