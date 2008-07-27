C
C $Id: opngks.f,v 1.8 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE OPNGKS
C
C IU(6), in IUTLCM, is the current metacode unit number, negated if the
C unit is currently in use.
C
      COMMON /IUTLCM/ IU(100)
      SAVE /IUTLCM/
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL UTILBD
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
