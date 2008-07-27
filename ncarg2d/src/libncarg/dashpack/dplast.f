C
C $Id: dplast.f,v 1.6 2008-07-27 00:16:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPLAST
C
C This routine generates the appropriate call to terminate a curve
C drawn by an initial call to DPFRST and one or more subsequent calls
C to DPVECT.
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPLAST - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Call the appropriate lower-level routine, depending on whether
C smoothing is turned on or not.
C
        IF (TENS.LT.0.) THEN
          CALL DPDRAW (0.,0.,2)
          IF (ICFELL('DPLAST',2).NE.0) RETURN
        ELSE
          CALL DPSMTH (0.,0.,2)
          IF (ICFELL('DPLAST',3).NE.0) RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
