C
C $Id: dpline.f,v 1.5 2000-08-22 15:03:23 haley Exp $
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
