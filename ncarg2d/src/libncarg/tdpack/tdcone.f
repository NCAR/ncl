C
C $Id: tdcone.f,v 1.1 2003-11-24 20:38:32 kennison Exp $
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
        RLAT=RTOD*ASIN(WDCS)
C
        IF (UDCS.NE.0.OR.VDCS.NE.0.) THEN
          RLON=RTOD*ATAN2(VDCS,UDCS)
        ELSE
          RLON=0.
        END IF
C
C Draw the circular base of the arrowhead.  Essentially, we form the
C circle in the UV plane, perform two rotations (one about the V axis
C and one about the W axis), and then translate the circle to its
C desired final position.
C
        DO 101 I=0,NPAC
          UCR1=RADI*COS(DTOR*REAL(I)*(360./REAL(NPAC)))
          UCR2=UCR1*WDCS
          VCR2=RADI*SIN(DTOR*REAL(I)*(360./REAL(NPAC)))
          UCR3=UCR2*UDCS-VCR2*VDCS
          VCR3=VCR2*UDCS+UCR2*VDCS
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
          UCR3=UCR2*UDCS-VCR2*VDCS
          VCR3=VCR2*UDCS+UCR2*VDCS
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
