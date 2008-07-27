C
C	$Id: hlsrgb.f,v 1.5 2008-07-27 00:16:53 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE HLSRGB ( H, L, S, R, G, B )
C
C_BEGIN PROLOGUE HLSRGB
C
C_PURPOSE
C    This subroutine converts a color specification given in
C    the Hue, Lightness, and Saturation color space to
C    Red, Green, and Blue intensity values in the RGB color
C    space.
C
C_DESCRIPTION
C    This subroutine converts color values given in the
C    Hue, Lightness, Saturation (HLS) color space to color values
C    in the Red, Green, and Blue (RGB) color space.
C    The input value H is a floating point number in the
C    range [0.,360.); a value of H=0. in the input space
C    will result in a full blue (red and green values of 0.)
C    in the RGB space.  The input value L is in the range
C    [0.,100.]; lightness is used to signify the amount of light--
C    L=0. will result in R=G=B=0. and L=1. will result in
C    R=G=B=1. regardless of H and S.  The input value S is
C    in the range [0.,100.];  saturation is a measure of the
C    mixture of white light with a pure fully-saturated hue--
C    S=0. will result in R=G=B=L.  The output values
C    R, G, and B are red, green, and blue intensities
C    in the range 0. to 1. .
C
C_ARGUMENTS
C
C H       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,360.) which represents the hue of the input color
C         in HLS color space.  H=0. corresponds to blue.
C
C L       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,100.] which represents the lightness value of the
C         input color in HLS color space.  Lightness is a measure
C         of the quantity of light--a lightness of 0. is black,
C         and a lightness of 100. gives white.  The pure hues occur
C         at lightness value 50.
C
C S       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,100.] which represents the saturation value of the
C         input color in HLS color space.  Saturation is a measure
C         of how much white light is mixed with the color. Colors
C         having a satuartion value of 0. represent greys
C         with a grey intensity value equal to the lightness L.
C         Colors with a saturation value of 100. are
C         fully saturated colors.  The hue is undefined
C         when S=0.  The fully saturated pure hues occur when S=100.
C         and L=50.
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
C    This subroutine was originally coded at NCAR by Roy Barnes
C    using the algorithm presented in Foley and van Dam (see
C    REFERENCES below); it was prepared for the NCAR Graphics
C    package by Fred Clare.
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
C    plot the red output values vs. hue for S=100. and L=50.  The red
C    values so plotted form a piecewise linear curve.  The blue
C    and green values plotted in the same way form the same
C    piecewise linear curve except shifted.  Decreasing the
C    saturation pushes the curves toward the lightness value,
C    and varying the lightness introduces an amplitude scale
C    factor and vertical offset.
C
C_EXAMPLES
C    Example 1 --  Pure hue to red.
C          SUBROUTINE HLS2
C          REAL    R,G,B,EPS
C          PARAMETER (EPS=0.00001)
C          CALL HLSRGB(120.,50.,100.,R,G,B)
C          IF ( (ABS(R-1.) .GT. EPS) .OR. (ABS(G) .GT. EPS) .OR.
C         *     (ABS(B) .GT. EPS) ) THEN
C            IUN = I1MACH(4)
C            WRITE(IUN,10)
C          ENDIF
C          RETURN
C       10 FORMAT(' HLS2 returned incorrect values')
C          END
C
C_END PROLOGUE HLSRGB
C
      REAL   H,HH,L,S,R,G,B,LL,SS,M1,M2,SIGPLC
C
C  Check on the input values.
C
C_FIRST EXECUTABLE STATEMENT HLSRGB
      HH = MOD(H,360.)
      IF (HH .LT. 0.) HH = HH+360.
      IF (L.LT.0. .OR. L.GT.100.)
     *      CALL SETER(' HLSRGB - L out of range',1,1)
      IF (S.LT.0. .OR. S.GT.100.)
     *      CALL SETER(' HLSRGB - S out of range',1,1)
C
C  Normalize lightness and saturation values.
C
      LL = L/100.
      SS = S/100.
      IF (SS .EQ. 0.0) THEN
C
C  Achromatic case; RGB value is a grey equal to the lightness.
C
         R = LL
         G = LL
         B = LL
      ELSE
C
C  Calculate the max and min of the generic piecewise linear
C  color component function.
C
         IF (LL .LE. 0.5) THEN
             M2 = LL * (1.0 + SS)
         ELSE
             M2 = LL + SS - (LL * SS)
         ENDIF
         M1 = 2.0 * LL - M2
C
C  Compute RGB values.
C
         R = SIGPLC(M1,M2,HH+  0.)
         G = SIGPLC(M1,M2,HH-120.)
         B = SIGPLC(M1,M2,HH+120.)
      ENDIF
      RETURN
      END
