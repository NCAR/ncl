C
C $Id: dpcurv.f,v 1.5 2000-08-22 15:03:22 haley Exp $
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
