C
C $Id: cpezct.f,v 1.2 1994-03-17 01:50:50 kennison Exp $
C
      SUBROUTINE CPEZCT (ZDAT,MZDT,NZDT)
C
      DIMENSION ZDAT(MZDT,NZDT)
C
C This routine simulates the old routine EZCNTR.
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPEZCT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Call CPCNRC to do the job.
C
      CALL CPCNRC (ZDAT,MZDT,MZDT,NZDT,0.,0.,0.,0,0,-682)
      IF (ICFELL('CPEZCT',2).NE.0) RETURN
C
C Advance the frame.
C
      CALL FRAME
      IF (ICFELL('CPEZCT',3).NE.0) RETURN
C
      RETURN
C
      END
