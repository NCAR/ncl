C
C $Id: pchiqu.f,v 1.4 1994-03-17 18:44:36 kennison Exp $
C
      SUBROUTINE PCHIQU (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This routine provides an alternate entry point for the high-quality
C character-drawing routine; the name has the standard prefix "PC".
C
        CHARACTER CHRS*(*)
C
C Check for an uncleared prior error.
C
        IF (ICFELL('PCHIQU - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        CALL PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
        IF (ICFELL('PCHIQU',2).NE.0) RETURN
C
        RETURN
C
      END
