C
C $Id: sfwrld.f,v 1.2 1994-03-17 20:58:55 kennison Exp $
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
C Check for an uncleared prior error.
C
      IF (ICFELL('SFWRLD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Convert the data to the proper units.
C
      DO 10001 I=1,NRA
      XRA(I)=CUFX(XRA(I))
      IF (ICFELL('SFWRLD',2).NE.0) RETURN
      YRA(I)=CUFY(YRA(I))
      IF (ICFELL('SFWRLD',3).NE.0) RETURN
10001 CONTINUE
C
C Call the routine SFNORM to finish the job.
C
      CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
      IF (ICFELL('SFWRLD',4).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
