C
C	$Id: cpezct.f,v 1.1.1.1 1992-04-17 22:32:44 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CPEZCT (ZDAT,MZDT,NZDT)
C
      DIMENSION ZDAT(MZDT,NZDT)
C
C This routine simulates the old routine EZCNTR.
C
      CALL CPCNRC (ZDAT,MZDT,MZDT,NZDT,0.,0.,0.,0,0,-682)
C
      CALL FRAME
C
      RETURN
C
      END
