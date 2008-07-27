C
C	$Id: sigplc.f,v 1.5 2008-07-27 00:16:54 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION SIGPLC(M1,M2,HUE)
C
C_BEGIN PROLOGUE SIGPLC
C
C_SUBSIDIARY HLSRGB, HSVRGB
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
C    880511 (YYMMDD)
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
C_END PROLOGUE SIGPLC
C
      REAL M1,M2,HUE,H
C_FIRST EXECUTABLE STATEMENT SIGPLC
      H = HUE
      H = MOD(H,360.)
      IF (H .LT. 0.) H = H+360.
      IF (H .LT. 60.0) THEN
C
C  Linear curve from M1 to M2 if HUE is in [0.,60.)
C
          SIGPLC = M1 + (M2 - M1) * H / 60.0
      ELSE IF (H .LT. 180.0) THEN
C
C  Maximum value for HUE in [60.,180.)
C
          SIGPLC = M2
      ELSE IF (H .LT. 240.0) THEN
C
C  Linear curve from M2 to M1 for HUE in [180.,240.)
C
          SIGPLC = M1 + (M2 - M1) * (240.0 - H) / 60.0
      ELSE
C
C  Minimum value for HUE in [240.,360.)
C
          SIGPLC = M1
      ENDIF
C
C  Guarantee SIGPLC is in the range 0. to 1.
C
      SIGPLC = MIN(1.,MAX(0.,SIGPLC))
      RETURN
      END
