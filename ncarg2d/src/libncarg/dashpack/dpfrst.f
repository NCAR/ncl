C
C $Id: dpfrst.f,v 1.6 2008-07-27 00:16:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
