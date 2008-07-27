C
C $Id: tdcone.f,v 1.3 2008-07-27 00:17:31 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDCONE (UACC,VACC,WACC,UDCS,VDCS,WDCS,RADI,NPAC,
     +                                       NPCT,UTIP,VTIP,WTIP)
C
C Draw the projection of a cone in 3-space.  The circular base of the
C cone is centered at the point (UACC,VACC,WACC), is perpendicular to
C the vector with direction cosines UDCS, VDCS, and WDCS, has a radius
C of RADI, and is drawn using NPAC equispaced points along the circle;
C NPCT points equispaced along the circle are connected to the tip,
C which is at (UTIP,VTIP,WTIP).
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C       RLAT=RTOD*ASIN(WDCS)
C
C       IF (UDCS.NE.0.OR.VDCS.NE.0.) THEN
C         RLON=RTOD*ATAN2(VDCS,UDCS)
C       ELSE
C         RLON=0.
C       END IF
C
C Draw the circular base of the arrowhead.  Essentially, we form the
C circle in the UV plane, perform two rotations (one about the V axis,
C by the angle 90-RLAT, and one about the W axis, by the angle RLON),
C and then translate the circle to its desired final position.  The
C rotations involve sines and cosines of 90-RLAT and of RLON; the code
C is simplified by computing these from the direction cosines.
C
        DNOM=SQRT(UDCS*UDCS+VDCS*VDCS)
C
        DO 101 I=0,NPAC
          UCR1=RADI*COS(DTOR*REAL(I)*(360./REAL(NPAC)))
          UCR2=UCR1*WDCS
          VCR2=RADI*SIN(DTOR*REAL(I)*(360./REAL(NPAC)))
          IF (DNOM.EQ.0.) THEN
            UCR3=UCR2
            VCR3=VCR2
          ELSE
            UCR3=(UCR2*UDCS-VCR2*VDCS)/DNOM
            VCR3=(VCR2*UDCS+UCR2*VDCS)/DNOM
          END IF
          WCR3=-UCR1*SQRT(1.-WDCS*WDCS)
          CALL TDPRPT (UACC+UCR3,VACC+VCR3,WACC+WCR3,XPOS,YPOS)
          CALL PLOTIF (CUFX(XPOS),CUFY(YPOS),MIN(I,1))
  101   CONTINUE
C
C Connect some points along the base of the circle to the tip, using
C very similar code.
C
        CALL TDPRPT (UTIP,VTIP,WTIP,XTIP,YTIP)
C
        XTIP=CUFX(XTIP)
        YTIP=CUFY(YTIP)
C
        DO 102 I=0,NPCT
          CALL PLOTIF (XTIP,YTIP,0)
          UCR1=RADI*COS(DTOR*REAL(I)*(360./REAL(NPCT)))
          UCR2=UCR1*WDCS
          VCR2=RADI*SIN(DTOR*REAL(I)*(360./REAL(NPCT)))
          IF (DNOM.EQ.0.) THEN
            UCR3=UCR2
            VCR3=VCR2
          ELSE
            UCR3=(UCR2*UDCS-VCR2*VDCS)/DNOM
            VCR3=(VCR2*UDCS+UCR2*VDCS)/DNOM
          END IF
          WCR3=-UCR1*SQRT(1.-WDCS*WDCS)
          CALL TDPRPT (UACC+UCR3,VACC+VCR3,WACC+WCR3,XPOS,YPOS)
          CALL PLOTIF (CUFX(XPOS),CUFY(YPOS),1)
  102   CONTINUE
C
C Flush the point-draw buffers.
C
        CALL PLOTIF (0.,0.,2)
C
C Done.
C
        RETURN
C
      END
