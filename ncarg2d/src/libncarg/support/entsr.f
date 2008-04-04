C
C $Id: entsr.f,v 1.6 2008-04-04 21:02:55 kennison Exp $
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
      SUBROUTINE ENTSR (IROLD,IRNEW)
C
C This routine returns, in IROLD, the current value of the recovery
C flag.  If the value of IRNEW is non-zero, ENTSR also resets the
C recovery flag: IRNEW = 1 activates recovery; IRNEW = 2 deactivates
C recovery.
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDAX.
C
        COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
        SAVE   /SECOMI/
C
        COMMON /SECOMC/ ERMSG
          CHARACTER*256 ERMSG
        SAVE   /SECOMC/
C
C Check for an illegal value of IRNEW.
C
        IF (IRNEW.LT.0.OR.IRNEW.GT.2)
     +     CALL SETER ('ENTSR - ILLEGAL VALUE OF THE RECOVERY FLAG',1,2)
C
C Return the previous value of IRECF in IROLD and, if IRNEW is nonzero,
C reset IRECF to that value.
C
        IROLD=IRECF
        IF (IRNEW.NE.0) IRECF=IRNEW
C
C Check for an uncleared prior error that is now unrecoverable.
C
        IF (IERRF.NE.0.AND.IRECF.EQ.2)
     +       CALL SETER ('ENTSR - PRIOR ERROR IS NOW UNRECOVERABLE',2,2)
C
C Done.
C
        RETURN
C
      END
