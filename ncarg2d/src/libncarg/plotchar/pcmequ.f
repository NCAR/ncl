C
C $Id: pcmequ.f,v 1.2 1994-03-09 23:23:48 kennison Exp $
C
      SUBROUTINE PCMEQU (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This routine provides an alternate entry point for the medium-quality
C character-drawing routine; the name has the standard prefix "PC".
C
        CHARACTER CHRS*(*)
C
        CALL PLCHMQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
        IF (ICFELL('PCMEQU',1).NE.0) RETURN
C
        RETURN
C
      END
