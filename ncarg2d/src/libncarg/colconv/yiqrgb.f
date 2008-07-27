C
C	$Id: yiqrgb.f,v 1.6 2008-07-27 00:16:54 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE YIQRGB ( Y, I, Q, R, G, B )
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
C Y       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] .  Y is the color component of a television
C         signal which is shown on black-and-white televisions;
C         Y minimizes the effect of two colors appearing different
C         to the human eye but mapping to similar monochrome
C         intensities.
C
C I       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [-.6,.6] .
C
C Q       (INPUT,SINGLE,VARIABLE)  A real variable in the range
C         [-.52,.52].
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
C          REAL    R,G,B,EPS
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
      REAL   R,G,B,Y,I,Q
C
C  Check on the input values.
C
C_FIRST EXECUTABLE STATEMENT YIQRGB
C
      R = Y +  .956*I +  .621*Q
      G = Y -  .272*I -  .647*Q
      B = Y - 1.105*I + 1.702*Q
      R = MIN(MAX(R,0.),1.)
      G = MIN(MAX(G,0.),1.)
      B = MIN(MAX(B,0.),1.)
      RETURN
      END
