C
C	$Id: hsvrgb.f,v 1.5 2008-07-27 00:16:53 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE HSVRGB ( H, S, V, R, G, B )
C
C_BEGIN PROLOGUE HSVRGB
C
C_PURPOSE
C    This subroutine converts a color specification given in
C    the Hue, Saturation, and Value color space to
C    Red, Green, and Blue intensity values in the RGB color
C    space.
C
C_DESCRIPTION
C    This subroutine converts color values given in the
C    Hue, Saturation, and Value (HSV) color space to color values
C    in the Red, Green, and Blue (RGB) color space.
C    The input value H is a floating point number in the
C    range [0.,360.); a value of H=0. in the input space
C    will result in a full red (green and blue values of 0.)
C    in the RGB space.  The input value V is in the range
C    [0.,1.];  the input value S is
C    in the range [0.,1.];  saturation is a measure of the
C    mixture of white light with a pure fully-saturated hue--
C    S=0. will result in R=G=B=V.  The output values
C    R, G, and B are red, green, and blue intensities
C    in the range [0.,1.] .
C
C_ARGUMENTS
C
C H       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,360.) which represents the hue of the input color
C         in HSV color space.
C
C S       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the saturation value of the
C         input color in HSV color space.  Saturation is a measure
C         of how much white light is mixed with the color. Saturation
C         values of 0. represent greys (with a grey value equal to
C         the value V).  Saturation values of 1. are fully
C         saturated colors.  The hue is undefined when S=0.  The
C         fully saturated pure hues occur when S=1. and V=1.
C
C V       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the value of the
C         input color in HSV color space.
C
C R       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the red intensity component
C         of the output color in RGB color space.
C
C G       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the green intensity component
C         of the output color in RGB color space.
C
C B       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the blue intensity component
C         of the output color in RGB color space.
C
C_I/O
C    (NONE)
C
C_ROUTINES CALLED
C    SIGPLC
C
C_COMMON BLOCKS
C    (NONE)
C
C_MAXIMUM GKS LEVEL
C    (NONE)
C
C_LANGUAGE
C    FORTRAN
C
C_REVISION DATE
C    880511 (YYMMDD)
C
C_HISTORY
C    This subroutine was originally coded and prepared for the
C    NCAR Graphics package by Fred Clare based on algorithms
C    in Foley and van Dam (see REFERENCES below).
C
C_REFERENCES
C    Foley, James D. and van Dam, Andries,"Fundamentals of Interactive
C    Computer Graphics",Addison-Wesley Publishing Company, 1982.
C
C    Smith, A.R.,"Color Gamut Transformation Pairs",SIGGRAPH '78
C    Proceedings, published as Computer Graphics, 12(3),August 1978,
C    pp.12-19.
C
C_LONG DESCRIPTION
C    The idea of the algorithm can be understood if one were to
C    plot the red output values vs. hue with S=1. and V=1.  The red
C    values so plotted form a piecewise linear curve.  The blue
C    and green values plotted in the same way form the same
C    piecewise linear curve except shifted.  Decreasing the
C    saturation pushes the curves toward the input value V,,
C    and varying the value, V, introduces an amplitude scale
C    factor.  Drawing plots of the red, green, and blue output
C    values vs. hue for selected values of S, and V should
C    provide an intuitive feel for the algorithm.
C
C_EXAMPLES
C    Example 1 --  Pure hue in HSV to green in RGB.
C          SUBROUTINE HSV2
C          REAL    R,G,B,EPS
C          PARAMETER (EPS=0.00001)
C          CALL HSVRGB(120.,1.,1.,R,G,B)
C          IF ( (ABS(R-0.) .GT. EPS) .OR. (ABS(G-1.) .GT. EPS) .OR.
C         *     (ABS(B-0.) .GT. EPS) ) THEN
C            IUN = I1MACH(4)
C            WRITE(IUN,10)
C          ENDIF
C          RETURN
C       10 FORMAT(' HSV2 returned incorrect values')
C          END
C
C_END PROLOGUE HSVRGB
C
      REAL   H,HH,S,V,R,G,B,SM1,SIGPLC
C
C  Check on the input values.
C
C_FIRST EXECUTABLE STATEMENT HSVRGB
      HH = MOD(H,360.)
      IF (HH .LT. 0.) HH = HH+360.
      IF (S.LT.0. .OR. S.GT.1.)
     *      CALL SETER(' HSVRGB - S out of range',1,1)
      IF (V.LT.0. .OR. V.GT.1.)
     *      CALL SETER(' HSVRGB - V out of range',1,1)
C
      IF (S .EQ. 0.0) THEN
C
C  Achromatic case; RGB value is a grey equal to V;
C  H is not defined.
C
         R = V
         G = V
         B = V
      ELSE
C
C  Calculate RGB values.
C
         SM1 = (1.-S)*V
         R = SIGPLC(SM1,V,HH+120.)
         G = SIGPLC(SM1,V,HH+  0.)
         B = SIGPLC(SM1,V,HH+240.)
      ENDIF
      RETURN
      END
