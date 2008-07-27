C
C $Id: tdcudp.f,v 1.2 2008-07-27 00:17:32 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDCUDP (UCRV,VCRV,WCRV,NCRV,IARH,ARHL,ARHW)
C
        DIMENSION UCRV(NCRV),VCRV(NCRV),WCRV(NCRV)
C
C Draw the projection of a curve in 3-space, as defined by the points
C (UCRV(I),VCRV(I),WCRV(I)), for I from 1 to NCRV.  This routine is just
C like TDCURV, except that it calls the DASHPACK routine DPDRAW instead
C of the SPPS routine PLOTIF and can therefore be made to draw a dashed
C line.  The points are defined by their positions in 3-space.
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
        IF (ICFELL('TDCUDP - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Project the points of the curve, convert to the fractional coordinate
C system, and draw the curve using calls to the DASHPACK routine DPDRAW.
C
        DO 101 I=1,NCRV
          CALL TDPRPT (UCRV(I),VCRV(I),WCRV(I),XPOS,YPOS)
          CALL DPDRAW (CUFX(XPOS),CUFY(YPOS),MIN(I-1,1))
  101   CONTINUE
C
C Flush the point-draw buffers.
C
        CALL DPDRAW (0.,0.,2)
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
