C
C $Id: pcloqu.f,v 1.1 1993-01-12 02:41:19 kennison Exp $
C
      SUBROUTINE PCLOQU (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This routine provides an alternate entry point for the high-quality
C character-drawing routine; the name has the standard prefix "PC".
C
        CHARACTER CHRS*(*)
C
        CALL PLCHLQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
        RETURN
C
      END
