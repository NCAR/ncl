C
C $Id: tdcurv.f,v 1.1 2003-11-24 20:38:32 kennison Exp $
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
      SUBROUTINE TDCURV (UCRV,VCRV,WCRV,NCRV,IARH,ARHL,ARHW)
C
        DIMENSION UCRV(NCRV),VCRV(NCRV),WCRV(NCRV)
C
C Draw the projection of a curve in 3-space, as defined by the points
C (UCRV(I),VCRV(I),WCRV(I)), for I from 1 to NCRV.
C
C In addition, if IARH is non-zero, draw, at the end of the curve, the
C projection of a cone-shaped arrowhead of length ARHL and width ARHW.
C The magnitude of IARH is of the form NPAC+1000*NPCT, where NPAC is the
C number of points to be used to draw the circular base of the cone and
C NPCT is the number of points on the base to be connected to the tip.
C NPAC is forced to be 32 or greater and NPCT is forced to be NPAC/4 or
C greater; therefore, if you use IARH=1, you'll get NPAC=32 and NPCT=8.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDCURV - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Project the points of the curve, convert to the fractional coordinate
C system, and draw the curve using calls to the SPPS routine PLOTIF.
C
        DO 101 I=1,NCRV
          CALL TDPRPT (UCRV(I),VCRV(I),WCRV(I),XPOS,YPOS)
          CALL PLOTIF (CUFX(XPOS),CUFY(YPOS),MIN(I-1,1))
  101   CONTINUE
C
C Flush the point-draw buffers.
C
        CALL PLOTIF (0.,0.,2)
C
C If an arrowhead is to be drawn at the end of the curve, do that.
C
        IF (IARH.NE.0.AND.NCRV.GE.2) THEN
          NPAC=MAX(32,MOD(IARH,1000))
          NPCT=MAX(NPAC/4,IARH/1000)
          DNOM=SQRT((UCRV(NCRV)-UCRV(NCRV-1))**2+
     +              (VCRV(NCRV)-VCRV(NCRV-1))**2+
     +              (WCRV(NCRV)-WCRV(NCRV-1))**2)
          IF (DNOM.NE.0.) THEN
            UDCS=(UCRV(NCRV)-UCRV(NCRV-1))/DNOM
            VDCS=(VCRV(NCRV)-VCRV(NCRV-1))/DNOM
            WDCS=(WCRV(NCRV)-WCRV(NCRV-1))/DNOM
            UACC=UCRV(NCRV)-ARHL*UDCS
            VACC=VCRV(NCRV)-ARHL*VDCS
            WACC=WCRV(NCRV)-ARHL*WDCS
            CALL TDCONE (UACC,VACC,WACC,UDCS,VDCS,WDCS,ARHW/2.,NPAC,
     +                   NPCT,UCRV(NCRV),VCRV(NCRV),WCRV(NCRV))
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
