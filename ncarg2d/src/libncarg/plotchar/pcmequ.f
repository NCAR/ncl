C
C $Id: pcmequ.f,v 1.4 1994-03-17 18:44:39 kennison Exp $
C
      SUBROUTINE PCMEQU (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This routine provides an alternate entry point for the medium-quality
C character-drawing routine; the name has the standard prefix "PC".
C
        CHARACTER CHRS*(*)
C
C Check for an uncleared prior error.
C
        IF (ICFELL('PCMEQU - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        CALL PLCHMQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
        IF (ICFELL('PCMEQU',2).NE.0) RETURN
C
        RETURN
C
      END
