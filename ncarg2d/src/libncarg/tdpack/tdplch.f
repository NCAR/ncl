C
C $Id: tdplch.f,v 1.1 1997-06-18 22:08:52 kennison Exp $
C
      SUBROUTINE TDPLCH (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
        CHARACTER CHRS*(*)
C
C This is a 3D entry for the routine PLCHHQ.  It is expected that the
C caller has done a call to TDPARA, defining a parallelogram in 3-space
C within which characters are to be placed.  It just sets the PLOTCHAR
C mapping flag to the appropriate value, calls PLCHHQ, and then resets
C the mapping flag to its original value.
C
        CALL PCGETI ('MAPPING FLAG',IMAP)
        CALL PCSETI ('MAPPING FLAG',3)
        CALL PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
        CALL PCSETI ('MAPPING FLAG',IMAP)
C
C Done.
C
        RETURN
C
      END
