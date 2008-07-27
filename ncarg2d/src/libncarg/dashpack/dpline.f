C
C $Id: dpline.f,v 1.6 2008-07-27 00:16:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPLINE (XCP1,YCP1,XCP2,YCP2)
C
C Given the user coordinates of the two points (XCP1,YCP1) and
C (XCP2,YCP2), this routine just draws a straight line connecting
C them.
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPLINE - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Just draw a line connecting two points.  Such lines are never
C smoothed.
C
        XTMP=CUFX(XCP1)
        IF (ICFELL('DPLINE',2).NE.0) RETURN
C
        YTMP=CUFY(YCP1)
        IF (ICFELL('DPLINE',3).NE.0) RETURN
C
        CALL DPDRAW (XTMP,YTMP,0)
        IF (ICFELL('DPLINE',4).NE.0) RETURN
C
        XTMP=CUFX(XCP2)
        IF (ICFELL('DPLINE',5).NE.0) RETURN
C
        YTMP=CUFY(YCP2)
        IF (ICFELL('DPLINE',6).NE.0) RETURN
C
        CALL DPDRAW (XTMP,YTMP,1)
        IF (ICFELL('DPLINE',7).NE.0) RETURN
C
        CALL DPDRAW (0.,0.,2)
        IF (ICFELL('DPLINE',8).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
