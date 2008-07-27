C
C	$Id: rgbhls.f,v 1.5 2008-07-27 00:16:54 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE RGBHLS ( R, G, B, H, L, S )
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
C R       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the red intensity component
C         of the input point in RGB color space.
C
C G       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the green intensity component
C         of the input point in RGB color space.
C
C B       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the blue intensity component
C         of the input point in RGB color space.
C
C H       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,360.) which represents the hue of the input point
C         in HLS color space.  A value of (0.,0.,B) in the input
C         space will result in a hue of 0. in the output space.
C
C L       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,100.] which represents the lightness value of the
C         input point in HLS color space.  Lightness is a measure
C         of the quantity of light--a lightness of 0. is black,
C         and a lightness of 100. gives white.  The pure hues occur
C         at lightness value 50.  The lightness should be thought
C         of as a percentage.
C
C S       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
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
C          REAL    H,L,S,EPS
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
      REAL R,G,B,H,L,S,MXINTS,MNINTS,MAXMIN,HSAV,BC,GC,RC,RMM
      SAVE HSAV
      DATA HSAV/0./
C
C  Check on input arguments.
C
C_FIRST EXECUTABLE STATEMENT RGBHLS
      IF (R.LT.0. .OR. R.GT.1.)
     *      CALL SETER(' RGBHLS - R out of range',1,1)
      IF (G.LT.0. .OR. G.GT.1.)
     *      CALL SETER(' RGBHLS - G out of range',1,1)
      IF (B.LT.0. .OR. B.GT.1.)
     *      CALL SETER(' RGBHLS - B out of range',1,1)
      MXINTS = MAX(R,G,B)
      MNINTS = MIN(R,G,B)
C
C  Compute lightness
C
      L = (MXINTS+MNINTS)*0.5
      IF (MXINTS .EQ. MNINTS) THEN
C
C  Achromatic case.  If the saturation is 0., then leave H
C  as it was in the previous call as per the suggestion of
C  A.V. Smith (see REFERENCES).
C
        S = 0.0
        H = HSAV
      ELSE
C
C  Chromatic case.
C
        MAXMIN = MXINTS-MNINTS
        IF (L .LE. 0.5) THEN
          S = MAXMIN/(MXINTS+MNINTS)
        ELSE
          S = MAXMIN/(2.0-MXINTS-MNINTS)
        ENDIF
        RMM = 1./MAXMIN
        RC = (MXINTS-R)*RMM
        GC = (MXINTS-G)*RMM
        BC = (MXINTS-B)*RMM
        IF (R .EQ. MXINTS) THEN
          H = 2.+BC-GC
        ELSE IF (G .EQ. MXINTS) THEN
          H = 4.+RC-BC
        ELSE IF (B .EQ. MXINTS) THEN
          H = 0.+GC-RC
        ENDIF
C
C  Convert H to degrees.
C
        H = H*60.
        IF (H .LT. 0.) H = H+360.
        HSAV = H
      ENDIF
C
C  Scale lightness and saturation values.
C
      L = L*100.
      S = S*100.
      L = MIN(100.,MAX(0.,L))
      S = MIN(100.,MAX(0.,S))
      RETURN
      END
