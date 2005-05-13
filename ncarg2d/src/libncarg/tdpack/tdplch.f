C
C $Id: tdplch.f,v 1.4 2005-05-13 20:00:24 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
