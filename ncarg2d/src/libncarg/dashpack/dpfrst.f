C
C $Id: dpfrst.f,v 1.4 2000-07-12 16:23:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE DPFRST (XCPU,YCPU)
C
C Given the user coordinates of the point (XCPU,YCPU), this routine
C generates the appropriate call to start a curve there; it is meant
C to be used in combination with DPVECT and DPLAST.
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPFRST - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Get the fractional-system coordinates of the point.
C
        XCPF=CUFX(XCPU)
        IF (ICFELL('DPFRST',2).NE.0) RETURN
C
        YCPF=CUFY(YCPU)
        IF (ICFELL('DPFRST',3).NE.0) RETURN
C
C Call the appropriate lower-level routine, depending on whether
C smoothing is turned on or not.
C
        IF (TENS.LT.0.) THEN
          CALL DPDRAW (XCPF,YCPF,0)
          IF (ICFELL('DPFRST',4).NE.0) RETURN
        ELSE
          CALL DPSMTH (XCPF,YCPF,0)
          IF (ICFELL('DPFRST',5).NE.0) RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
