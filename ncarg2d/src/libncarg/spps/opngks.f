C
C $Id: opngks.f,v 1.6 2006-03-10 00:25:35 kennison Exp $
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
      SUBROUTINE OPNGKS
C
C IU(6), in IUTLCM, is the current metacode unit number, negated if the
C unit is currently in use.
C
      COMMON /IUTLCM/ IU(100)
      SAVE /IUTLCM/
C
C Force the required BLOCKDATA to load.
C
      EXTERNAL UTILBD
C
C Make sure that, if there is an uncleared prior error, the error
C message gets printed.  Go ahead and open GKS, though.
C
      IERR=ICFELL('OPNGKS - UNCLEARED PRIOR ERROR',1)
C
C Open GKS, define a workstation, and activate the workstation.
C
      IF (IU(6).GT.0) THEN
        CALL GOPKS (6,0)
        CALL GOPWK (1,IU(6),1)
        CALL GACWK (1)
        IU(6)=-ABS(IU(6))
      END IF
C
      RETURN
C
      END
