C
C $Id: tdplch.f,v 1.5 2008-07-27 00:17:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
        CALL PCGETI ('MA - MAPPING FLAG      ',IMAP)
        CALL PCGETR ('OR - OUT-OF-RANGE VALUE',OORV)
        CALL PCSETI ('MA - MAPPING FLAG      ',3)
        CALL PCSETR ('OR - OUT-OF-RANGE VALUE',1.E12)
        CALL PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
        CALL PCSETI ('MA - MAPPING FLAG      ',IMAP)
        CALL PCSETR ('OR - OUT-OF-RANGE VALUE',OORV)
C
C Done.
C
        RETURN
C
      END
