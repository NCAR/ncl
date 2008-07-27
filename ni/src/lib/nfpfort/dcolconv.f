C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DHLSRGB(H,L,S,R,G,B)
C
C_BEGIN PROLOGUE DHLSRGB
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
C H       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,360.) which represents the hue of the input color
C         in HLS color space.  H=0. corresponds to blue.
C
C L       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,100.] which represents the lightness value of the
C         input color in HLS color space.  Lightness is a measure
C         of the quantity of light--a lightness of 0. is black,
C         and a lightness of 100. gives white.  The pure hues occur
C         at lightness value 50.
C
C S       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
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
C R       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the red intensity component
C         of the output color in RGB color space.
C
C G       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the green intensity component
C         of the output color in RGB color space.
C
C B       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the blue intensity component
C         of the output color in RGB color space.
C
C_I/O
C    (NONE)
C
C_ROUTINES CALLED
C    DSIGPLC
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
C          DOUBLE PRECISION  R,G,B,EPS
C          PARAMETER (EPS=0.00001)
C          CALL DHLSRGB(120.,50.,100.,R,G,B)
C          IF ( (ABS(R-1.) .GT. EPS) .OR. (ABS(G) .GT. EPS) .OR.
C         *     (ABS(B) .GT. EPS) ) THEN
C            IUN = I1MACH(4)
C            WRITE(IUN,10)
C          ENDIF
C          RETURN
C       10 FORMAT(' HLS2 returned incorrect values')
C          END
C
C_END PROLOGUE DHLSRGB
C
      DOUBLE PRECISION H,HH,L,S,R,G,B,LL,SS,M1,M2,DSIGPLC
C
C  Check on the input values.
C
C_FIRST EXECUTABLE STATEMENT DHLSRGB
      HH = MOD(H,360.D0)
      IF (HH.LT.0.D0) HH = HH + 360.D0
      IF (L.LT.0.D0 .OR. L.GT.100.D0)
     +    CALL SETER(' DHLSRGB - L out of range',1,1)
      IF (S.LT.0.D0 .OR. S.GT.100.D0)
     +    CALL SETER(' DHLSRGB - S out of range',1,1)
C
C  Normalize lightness and saturation values.
C
      LL = L/100.D0
      SS = S/100.D0
      IF (SS.EQ.0.0D0) THEN
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
          IF (LL.LE.0.5D0) THEN
              M2 = LL* (1.0D0+SS)
          ELSE
              M2 = LL + SS - (LL*SS)
          END IF
          M1 = 2.0D0*LL - M2
C
C  Compute RGB values.
C
          R = DSIGPLC(M1,M2,HH+0.D0)
          G = DSIGPLC(M1,M2,HH-120.D0)
          B = DSIGPLC(M1,M2,HH+120.D0)
      END IF
      RETURN
      END
      SUBROUTINE DHSVRGB(H,S,V,R,G,B)
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
C H       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,360.) which represents the hue of the input color
C         in HSV color space.
C
C S       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the saturation value of the
C         input color in HSV color space.  Saturation is a measure
C         of how much white light is mixed with the color. Saturation
C         values of 0. represent greys (with a grey value equal to
C         the value V).  Saturation values of 1. are fully
C         saturated colors.  The hue is undefined when S=0.  The
C         fully saturated pure hues occur when S=1. and V=1.
C
C V       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the value of the
C         input color in HSV color space.
C
C R       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the red intensity component
C         of the output color in RGB color space.
C
C G       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the green intensity component
C         of the output color in RGB color space.
C
C B       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the blue intensity component
C         of the output color in RGB color space.
C
C_I/O
C    (NONE)
C
C_ROUTINES CALLED
C    DSIGPLC
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
C          DOUBLE PRECISION    R,G,B,EPS
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
      DOUBLE PRECISION H,HH,S,V,R,G,B,SM1,DSIGPLC
C
C  Check on the input values.
C
C_FIRST EXECUTABLE STATEMENT HSVRGB
      HH = MOD(H,360.D0)
      IF (HH.LT.0.D0) HH = HH + 360.D0
      IF (S.LT.0.D0 .OR. S.GT.1.D0) CALL SETER
     +    (' HSVRGB - S out of range',1,1)
      IF (V.LT.0.D0 .OR. V.GT.1.D0) CALL SETER
     +    (' HSVRGB - V out of range',1,1)
C
      IF (S.EQ.0.0D0) THEN
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
          SM1 = (1.D0-S)*V
          R = DSIGPLC(SM1,V,HH+120.D0)
          G = DSIGPLC(SM1,V,HH+0.D0)
          B = DSIGPLC(SM1,V,HH+240.D0)
      END IF
      RETURN
      END
C
C     $Id: dcolconv.f,v 1.2 2008-07-27 03:40:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
      SUBROUTINE DRGBHLS(R,G,B,H,L,S)
C
C_BEGIN PROLOGUE RGBHLS
C
C_PURPOSE
C    This subroutine converts a color specification given as
C    Red, Green, and Blue intensity values to a color specification
C    given as Hue, Lightness, and Saturation values.
C
C_DESCRIPTION
C    This subroutine converts color values given in the
C    Red, Green, Blue (RGB) color space to color values
C    in the Hue, Lightness, and Saturation (HLS) color space.
C    The input Red, Green, and Blue values are intensities
C    in the range [0.,1.]; the output value H is returned
C    as a floating point number in the range [0.,360.);
C    full blue in the input space (red and green values of 0.)
C    corresponds to a hue of 0. in the
C    output space; the output value L is returned as a
C    floating point number in the range [0.,100.];
C    the output value S is returned as a floating
C    point number in the range [0.,100.] .
C
C_ARGUMENTS
C
C R       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the red intensity component
C         of the input point in RGB color space.
C
C G       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the green intensity component
C         of the input point in RGB color space.
C
C B       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the blue intensity component
C         of the input point in RGB color space.
C
C H       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,360.) which represents the hue of the input point
C         in HLS color space.  A value of (0.,0.,B) in the input
C         space will result in a hue of 0. in the output space.
C
C L       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,100.] which represents the lightness value of the
C         input point in HLS color space.  Lightness is a measure
C         of the quantity of light--a lightness of 0. is black,
C         and a lightness of 100. gives white.  The pure hues occur
C         at lightness value 50.  The lightness should be thought
C         of as a percentage.
C
C S       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,100.] which represents the saturation value of the
C         input point in HLS color space.  Saturation is a measure
C         of how much white light is mixed with the color.  Saturation
C         values of 0. represent greys (with a grey value equal to
C         the lightness value L).  Saturation values of 100. are fully
C         saturated colors.  The hue is undefined when S=0.  The
C         fully saturated pure hues occur when S=100. and L=50.
C         The saturation value should be thought of as a percentage.
C
C_I/O
C    (NONE)
C
C_ROUTINES CALLED
C    (NONE)
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
C    based on the algorithm presented in Foley and van Dam (see
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
C    The hues returned can be made to put 0 degrees at red instead
C    of blue by changing the offsets to the computation of H in the
C    code.  Hue 0. representing blue, and S and L values ranging
C    from 0. to 100. instead of 0. to 1. were chosen for compatability
C    with the Tektronix color model, and compatibility with the
C    NCAR FORTRAN CGM translator GRAPHCAPs.
C
C_EXAMPLES
C    Example 1 --  Pure red at full intensity to HLS
C          SUBROUTINE HLS1
C          DOUBLE PRECISION    H,L,S,EPS
C          PARAMETER (EPS=0.00001)
C          CALL RGBHLS(1.,0.,0.,H,L,S)
C          IF ( (ABS(H-120.) .GT. EPS) .OR. (ABS(L-50.) .GT. EPS) .OR.
C         *     (ABS(S-100.) .GT. EPS) ) THEN
C            IUN = I1MACH(4)
C            WRITE(IUN,10)
C          ENDIF
C          RETURN
C       10 FORMAT(' HLS1 returned incorrect values')
C          END
C
C_END PROLOGUE RGBHLS
C
      DOUBLE PRECISION R,G,B,H,L,S,MXINTS,MNINTS,MAXMIN,HSAV,BC,GC,RC,
     +                 RMM
      SAVE HSAV
      DATA HSAV/0.D0/
C
C  Check on input arguments.
C
C_FIRST EXECUTABLE STATEMENT RGBHLS
      IF (R.LT.0.D0 .OR. R.GT.1.D0) CALL SETER
     +    (' RGBHLS - R out of range',1,1)
      IF (G.LT.0.D0 .OR. G.GT.1.D0) CALL SETER
     +    (' RGBHLS - G out of range',1,1)
      IF (B.LT.0.D0 .OR. B.GT.1.D0) CALL SETER
     +    (' RGBHLS - B out of range',1,1)
      MXINTS = MAX(R,G,B)
      MNINTS = MIN(R,G,B)
C
C  Compute lightness
C
      L = (MXINTS+MNINTS)*0.5D0
      IF (MXINTS.EQ.MNINTS) THEN
C
C  Achromatic case.  If the saturation is 0., then leave H
C  as it was in the previous call as per the suggestion of
C  A.V. Smith (see REFERENCES).
C
          S = 0.0D0
          H = HSAV
      ELSE
C
C  Chromatic case.
C
          MAXMIN = MXINTS - MNINTS
          IF (L.LE.0.5D0) THEN
              S = MAXMIN/ (MXINTS+MNINTS)
          ELSE
              S = MAXMIN/ (2.0D0-MXINTS-MNINTS)
          END IF
          RMM = 1.D0/MAXMIN
          RC = (MXINTS-R)*RMM
          GC = (MXINTS-G)*RMM
          BC = (MXINTS-B)*RMM
          IF (R.EQ.MXINTS) THEN
              H = 2.D0 + BC - GC
          ELSE IF (G.EQ.MXINTS) THEN
              H = 4.D0 + RC - BC
          ELSE IF (B.EQ.MXINTS) THEN
              H = 0.D0 + GC - RC
          END IF
C
C  Convert H to degrees.
C
          H = H*60.D0
          IF (H.LT.0.D0) H = H + 360.D0
          HSAV = H
      END IF
C
C  Scale lightness and saturation values.
C
      L = L*100.D0
      S = S*100.D0
      L = MIN(100.D0,MAX(0.D0,L))
      S = MIN(100.D0,MAX(0.D0,S))
      RETURN
      END
C
C     $Id: dcolconv.f,v 1.2 2008-07-27 03:40:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DRGBHSV(R,G,B,H,S,V)
C
C_BEGIN PROLOGUE RGBHSV
C
C_PURPOSE
C    This subroutine converts a color specification given as
C    Red, Green, and Blue intensity values to a color specification
C    in the Hue, Saturation, Value color space.
C
C_DESCRIPTION
C    This subroutine converts color values given in the
C    Red, Green, Blue (RGB) color space to color values
C    in the Hue, Saturation, and Value (HSV) color space.
C    The input Red, Green, and Blue values are intensities
C    in the range [0.,1.]; the output value H is returned
C    as a floating point number in the range [0.,360.) .
C    Full red (green and blue values of 0.) in the
C    input space corresponds to a hue of 0 in the
C    output space; the output value V is returned as a
C    floating point number in the range [0.,1.];
C    the output value S is returned as a floating
C    point number in the range [0.,1.] .
C
C_ARGUMENTS
C
C R       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the red intensity component
C         of the input point in RGB color space.
C
C G       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the green intensity component
C         of the input point in RGB color space.
C
C B       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the blue intensity component
C         of the input point in RGB color space.
C
C H       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,360.) which represents the hue of the input point
C         in HSV color space.  A value of (R,0.,0.) in the input
C         space will result in a hue of 0. in the output space.
C
C S       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the saturation value of the
C         input point in HSV color space.  Saturation is a measure
C         of how much white light is mixed with the color. Saturation
C         values of 0. represent greys (with a grey value equal to
C         V).  Saturation values of 1. are fully saturated colors.
C         The hue is technically undefined when S=0; the code leaves
C         H at its previous value when S=0. (0. initially).  The
C         fully saturated pure hues occur when S=1. and V=1.
C
C V       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the value in HSV space.
C
C_I/O
C    (NONE)
C
C_ROUTINES CALLED
C    (NONE)
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
C    The hues returned can be made to put 0 degrees at blue instead
C    of red by changing the offsets to the computation of H in the
C    code.
C
C_EXAMPLES
C    Example 1 --  Blue to HSV.
C          SUBROUTINE THSV1
C          DOUBLE PRECISION    H,S,V,EPS
C          PARAMETER (EPS=0.00001)
C          CALL RGBHSV(0.,0.,1.,H,S,V)
C          IF ( (ABS(H-240.) .GT. EPS) .OR. (ABS(S-1.) .GT. EPS) .OR.
C         *     (ABS(V-1.) .GT. EPS) ) THEN
C            IUN = I1MACH(4)
C            WRITE(IUN,10)
C          ENDIF
C          RETURN
C       10 FORMAT(' THSV1 returned incorrect values')
C          END
C
C_END PROLOGUE RGBHSV
C
      DOUBLE PRECISION R,G,B,H,S,V,HSAV,RMM,MXINTS,MNINTS,MAXMIN,BC,GC,
     +                 RC
      SAVE HSAV
      DATA HSAV/0.D0/
C
C  Check on input arguments.
C
C_FIRST EXECUTABLE STATEMENT RGBHSV
      IF (R.LT.0.D0 .OR. R.GT.1.D0) CALL SETER
     +    (' RGBHSV - R out of range',1,1)
      IF (G.LT.0.D0 .OR. G.GT.1.D0) CALL SETER
     +    (' RGBHSV - G out of range',1,1)
      IF (B.LT.0.D0 .OR. B.GT.1.D0) CALL SETER
     +    (' RGBHSV - B out of range',1,1)
C
      MXINTS = MAX(R,G,B)
      MNINTS = MIN(R,G,B)
      MAXMIN = MXINTS - MNINTS
C
C  Compute value.
C
      V = MXINTS
C
C  Compute saturation.
C
      IF (MXINTS.EQ.0.D0) THEN
          S = 0.D0
      ELSE
          S = MAXMIN/MXINTS
      END IF
C
C  Compute hue.  If the saturation is 0., then do not define the hue--
C  leave it as it was in the previous call as per the suggestion of
C  A.V. Smith (see REFERENCES).
C
      IF (S.GT.0.D0) THEN
          RMM = 1.D0/MAXMIN
          RC = (MXINTS-R)*RMM
          GC = (MXINTS-G)*RMM
          BC = (MXINTS-B)*RMM
          IF (R.EQ.MXINTS) THEN
              H = 0.D0 + BC - GC
          ELSE IF (G.EQ.MXINTS) THEN
              H = 2.D0 + RC - BC
          ELSE IF (B.EQ.MXINTS) THEN
              H = 4.D0 + GC - RC
          END IF
C
C  Convert H to degrees.
C
          H = H*60.D0
          IF (H.LT.0.D0) H = H + 360.D0
          HSAV = H
      ELSE
          H = HSAV
      END IF
C
C  Guarantee saturation and value are in range.
C
      S = MIN(1.D0,MAX(0.D0,S))
      V = MIN(1.D0,MAX(0.D0,V))
      RETURN
      END
C
      SUBROUTINE DRGBYIQ(R,G,B,Y,I,Q)
C
C_BEGIN PROLOGUE RGBYIQ
C
C_PURPOSE
C    This subroutine converts a color specification given as
C    Red, Green, and Blue intensity values to a color specification
C    in the YIQ color space.
C
C_DESCRIPTION
C    This subroutine converts color values given in the
C    Red, Green, Blue (RGB) color space to color values
C    in the YIQ color space.  The input Red, Green,
C    and Blue values are intensities in the range [0.,1.] .
C
C_ARGUMENTS
C
C R       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the red intensity component
C         of the input point in RGB color space.
C
C G       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the green intensity component
C         of the input point in RGB color space.
C
C B       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the blue intensity component
C         of the input point in RGB color space.
C
C Y       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] .  Y is the color component of a television
C         signal which is shown on black-and-white televisions;
C         Y minimizes the effect of two colors appearing different
C         to the human eye but mapping to similar monochrome
C         intensities.
C
C I       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [-.6,.6] .  I attains its maximum when the input triple
C         is (1.,0.,0.); I attains its minimum when the input triple
C         is (0.,1.,1.) .
C
C Q       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [-.52,.52].  Q attains its maximum when the input triple
C         is (1.,0.,1.); Q attains its minimum when the input triple
C         is (0.,1.,0.).
C
C_I/O
C    (NONE)
C
C_ROUTINES CALLED
C    (NONE)
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
C_EXAMPLES
C    Example 1 --  Full white to YIQ.
C          SUBROUTINE TYIQ1
C          DOUBLE PRECISION    Y,I,Q,EPS
C          PARAMETER (EPS=0.01)
C          CALL RGBYIQ(1.,1.,1.,Y,I,Q)
C          IF ( (ABS(Y-1.) .GT. EPS) .OR. (ABS(I) .GT. EPS) .OR.
C         *     (ABS(Q) .GT. EPS) ) THEN
C            IUN = I1MACH(4)
C            WRITE(IUN,10)
C          ENDIF
C          RETURN
C       10 FORMAT(' TYIQ1 returned incorrect values')
C          END
C
C_END PROLOGUE RGBYIQ
C
      DOUBLE PRECISION R,G,B,Y,I,Q
C
C_FIRST EXECUTABLE STATEMENT RGBYIQ
      Y = 0.2988922D0*R + 0.5870147D0*G + 0.1140931D0*B
      I = 0.5960695D0*R - 0.2743087D0*G - 0.3217608D0*B
      Q = 0.2113775D0*R - 0.5229881D0*G + 0.3116106D0*B
C
      RETURN
      END
C
      SUBROUTINE DYIQRGB(Y,I,Q,R,G,B)
C
C_BEGIN PROLOGUE YIQRGB
C
C_PURPOSE
C    This subroutine converts a color specification given in
C    the YIQ coordinate system to the equivalent color
C    specification in the Red, Green, Blue coordinate system.
C
C_DESCRIPTION
C    This subroutine converts color values given in the
C    YIQ coordinate system to colors in the RGB coordinate
C    system.  The input value Y is in the range [0.,1.];
C    I is in the range [-.6,.6]; Q is in the range [-.52,.52].
C    The output values R, G, and B are red, green, and blue
C    intensities in the range [0.,1.] .
C
C_ARGUMENTS
C
C Y       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] .  Y is the color component of a television
C         signal which is shown on black-and-white televisions;
C         Y minimizes the effect of two colors appearing different
C         to the human eye but mapping to similar monochrome
C         intensities.
C
C I       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [-.6,.6] .
C
C Q       (INPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [-.52,.52].
C
C R       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the red intensity component
C         of the output color in RGB color space.
C
C G       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the green intensity component
C         of the output color in RGB color space.
C
C B       (OUTPUT,DOUBLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the blue intensity component
C         of the output color in RGB color space.
C
C_I/O
C    (NONE)
C
C_ROUTINES CALLED
C    (NONE)
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
C_EXAMPLES
C    Example 1 --  Convert green from YIQ to RGB.
C          SUBROUTINE TYIQ2
C          DOUBLE PRECISION    R,G,B,EPS
C          PARAMETER (EPS=0.01)
C          CALL YIQRGB(.59,-.28,-.52,R,G,B)
C          IF ( (ABS(R-0.) .GT. EPS) .OR. (ABS(G-1.) .GT. EPS) .OR.
C         *     (ABS(B-0.) .GT. EPS) ) THEN
C            IUN = I1MACH(4)
C            WRITE(IUN,10)
C          ENDIF
C          RETURN
C       10 FORMAT(' TYIQ2 returned incorrect values')
C          END
C
C_END PROLOGUE YIQRGB
C
      DOUBLE PRECISION R,G,B,Y,I,Q
C
C  Check on the input values.
C
C_FIRST EXECUTABLE STATEMENT YIQRGB
C
      R = Y + .956D0*I + .621D0*Q
      G = Y - .272D0*I - .647D0*Q
      B = Y - 1.105D0*I + 1.702D0*Q
      R = MIN(MAX(R,0.D0),1.D0)
      G = MIN(MAX(G,0.D0),1.D0)
      B = MIN(MAX(B,0.D0),1.D0)
      RETURN
      END
      DOUBLE PRECISION FUNCTION DSIGPLC(M1,M2,HUE)
C
C_BEGIN PROLOGUE DSIGPLC
C
C_SUBSIDIARY DHLSRGB, DHSVRGB
C
C_PURPOSE
C    To define a generic piecewise linear curve (as a function
C    of HUE) which can be used to calculate the R, G, B
C    values.
C
C_DESCRIPTION
C    This subroutine takes a minimum and maximum value and
C    defines a piecewise linear curve as a function of HUE.
C
C_ARGUMENTS
C
C M1      (INPUT,SINGLE,VARIABLE)  The minimum value.
C
C M2      (INPUT,SINGLE,VARIABLE)  The maximum value.
C
C HUE     (INPUT,SINGLE,VARIABLE)  Hue.
C
C_I/O
C    (NONE)
C
C_ROUTINES CALLED
C    (NONE)
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
C    070803 (YYMMDD)
C
C_HISTORY
C    This subroutine was originally coded at NCAR by Roy Barnes
C    using the algorithm presented in Foley and van Dam (see
C    REFERENCES); it was prepared for the NCAR Graphics
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
C_END PROLOGUE DSIGPLC
C
      DOUBLE PRECISION M1,M2,HUE,H
C_FIRST EXECUTABLE STATEMENT DSIGPLC
      H = HUE
      H = MOD(H,360.D0)
      IF (H.LT.0.D0) H = H + 360.D0
      IF (H.LT.60.0D0) THEN
C
C  Linear curve from M1 to M2 if HUE is in [0.,60.)
C
          DSIGPLC = M1 + (M2-M1)*H/60.0D0
      ELSE IF (H.LT.180.0D0) THEN
C
C  Maximum value for HUE in [60.,180.)
C
          DSIGPLC = M2
      ELSE IF (H.LT.240.0D0) THEN
C
C  Linear curve from M2 to M1 for HUE in [180.,240.)
C
          DSIGPLC = M1 + (M2-M1)* (240.0D0-H)/60.0D0
      ELSE
C
C  Minimum value for HUE in [240.,360.)
C
          DSIGPLC = M1
      END IF
C
C  Guarantee DSIGPLC is in the range 0. to 1.
C
      DSIGPLC = MIN(1.D0,MAX(0.D0,DSIGPLC))
      RETURN
      END
