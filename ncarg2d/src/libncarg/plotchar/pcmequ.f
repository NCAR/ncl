C
C $Id: pcmequ.f,v 1.1 1993-01-12 02:41:21 kennison Exp $
C
      SUBROUTINE PCMEQU (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This routine provides an alternate entry point for the medium-quality
C character-drawing routine; the name has the standard prefix "PC".
C
        CHARACTER CHRS*(*)
C
        CALL PLCHMQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
        RETURN
C
      END
