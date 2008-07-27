C
C $Id: dpcurv.f,v 1.6 2008-07-27 00:16:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPCURV (XCPU,YCPU,NPTS)
C
C The routine DPCURV, given the user-system coordinates of NPTS points
C in the arrays XCPU and YCPU, draws the curve from point 1 to point 2
C to point 3 ... to point NPTS.
C
        DIMENSION XCPU(NPTS),YCPU(NPTS)
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPCURV - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Draw a curve.  The coordinates of the points defining the curve are
C transformed into the fractional system and then passed either to
C DPDRAW (if smoothing is turned off) or to DPSMTH (if smoothing is
C turned on).
C
        DO 101 I=1,NPTS
C
          XTMP=CUFX(XCPU(I))
          IF (ICFELL('DPCURV',2).NE.0) RETURN
C
          YTMP=CUFY(YCPU(I))
          IF (ICFELL('DPCURV',3).NE.0) RETURN
C
          IF (TENS.LT.0.) THEN
            CALL DPDRAW (XTMP,YTMP,MIN(1,I-1))
            IF (ICFELL('DPCURV',4).NE.0) RETURN
          ELSE
            CALL DPSMTH (XTMP,YTMP,MIN(1,I-1))
            IF (ICFELL('DPCURV',5).NE.0) RETURN
          END IF
C
  101   CONTINUE
C
        IF (TENS.LT.0.) THEN
          CALL DPDRAW (0.,0.,2)
          IF (ICFELL('DPCURV',6).NE.0) RETURN
        ELSE
          CALL DPSMTH (0.,0.,2)
          IF (ICFELL('DPCURV',7).NE.0) RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
