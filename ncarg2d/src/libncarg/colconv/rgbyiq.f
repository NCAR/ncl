C
C	$Id: rgbyiq.f,v 1.7 2008-07-27 00:16:54 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE RGBYIQ ( R, G, B, Y, I, Q )
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
C Y       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
C         [0.,1.] .  Y is the color component of a television
C         signal which is shown on black-and-white televisions;
C         Y minimizes the effect of two colors appearing different
C         to the human eye but mapping to similar monochrome
C         intensities.
C
C I       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
C         [-.6,.6] .  I attains its maximum when the input triple
C         is (1.,0.,0.); I attains its minimum when the input triple
C         is (0.,1.,1.) .
C
C Q       (OUTPUT,SINGLE,VARIABLE)  A real variable in the range
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
C          REAL    Y,I,Q,EPS
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
      REAL R,G,B,Y,I,Q
C
C_FIRST EXECUTABLE STATEMENT RGBYIQ
      Y = 0.2988922*R +  0.5870147*G + 0.1140931*B
      I = 0.5960695*R -  0.2743087*G - 0.3217608*B
      Q = 0.2113775*R -  0.5229881*G + 0.3116106*B
C
      RETURN
      END
