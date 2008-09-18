C
C $Id: mdpiq.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPIQ
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCMP/  NPTB,XPTB(50),YPTB(50)
        INTEGER          NPTB
        REAL             XPTB,YPTB
        SAVE   /MAPCMP/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPIQ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Flush the points buffer.
C
        IF (NPTB.GT.0) THEN
          CALL POINTS (XPTB,YPTB,NPTB,0,0)
          IF (ICFELL('MDPIQ',2).NE.0) RETURN
          NPTB=0
        END IF
C
C Flush PLOTIT's buffer, too.
C
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('MDPIQ',3).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
