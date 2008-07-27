C
C	$Id: rgbhsv.f,v 1.6 2008-07-27 00:16:54 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE RGBHSV ( R, G, B, H, S, V )
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
C         in HSV color space.  A value of (R,0.,0.) in the input
C         space will result in a hue of 0. in the output space.
C
C S       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] which represents the saturation value of the
C         input point in HSV color space.  Saturation is a measure
C         of how much white light is mixed with the color. Saturation
C         values of 0. represent greys (with a grey value equal to
C         V).  Saturation values of 1. are fully saturated colors.
C         The hue is technically undefined when S=0; the code leaves
C         H at its previous value when S=0. (0. initially).  The
C         fully saturated pure hues occur when S=1. and V=1.
C
C V       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
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
C          REAL    H,S,V,EPS
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
      REAL R,G,B,H,S,V,HSAV,RMM,MXINTS,MNINTS,MAXMIN,BC,GC,RC
      SAVE HSAV
      DATA HSAV/0./
C
C  Check on input arguments.
C
C_FIRST EXECUTABLE STATEMENT RGBHSV
      IF (R.LT.0. .OR. R.GT.1.)
     *      CALL SETER(' RGBHSV - R out of range',1,1)
      IF (G.LT.0. .OR. G.GT.1.)
     *      CALL SETER(' RGBHSV - G out of range',1,1)
      IF (B.LT.0. .OR. B.GT.1.)
     *      CALL SETER(' RGBHSV - B out of range',1,1)
C
      MXINTS = MAX(R,G,B)
      MNINTS = MIN(R,G,B)
      MAXMIN = MXINTS-MNINTS
C
C  Compute value.
C
      V = MXINTS
C
C  Compute saturation.
C
      IF (MXINTS .EQ. 0.) THEN
        S = 0.
      ELSE
        S = MAXMIN/MXINTS
      ENDIF
C
C  Compute hue.  If the saturation is 0., then do not define the hue--
C  leave it as it was in the previous call as per the suggestion of
C  A.V. Smith (see REFERENCES).
C
      IF (S .GT. 0.) THEN
        RMM = 1./MAXMIN
        RC = (MXINTS-R)*RMM
        GC = (MXINTS-G)*RMM
        BC = (MXINTS-B)*RMM
        IF (R .EQ. MXINTS) THEN
          H = 0.+BC-GC
        ELSE IF (G .EQ. MXINTS) THEN
          H = 2.+RC-BC
        ELSE IF (B .EQ. MXINTS) THEN
          H = 4.+GC-RC
        ENDIF
C
C  Convert H to degrees.
C
        H = H*60.
        IF (H .LT. 0.) H = H+360.
        HSAV = H
      ELSE
        H = HSAV
      ENDIF
C
C  Guarantee saturation and value are in range.
C
      S = MIN(1.,MAX(0.,S))
      V = MIN(1.,MAX(0.,V))
      RETURN
      END
