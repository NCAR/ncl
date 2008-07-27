C
C $Id: ilsint.f,v 1.4 2008-07-27 00:17:31 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION ILSINT (XCPA,YCPA,XCPB,YCPB,XCPC,YCPC,XCPD,YCPD)
C
C This function has a non-zero value if and only if the 2D line
C segments AB and CD overlap.
C
C Initialize the value of the function to say that the line
C segments don't intersect.
C
        ILSINT=0
C
C Compute a denominator needed below.  (Its value is zero if and
C only if the line segments are parallel.)
C
        DNOM=(XCPB-XCPA)*(YCPD-YCPC)-(XCPD-XCPC)*(YCPB-YCPA)
C
C If the line segments are parallel, they don't intersect.
C
        IF (DNOM.EQ.0.) RETURN
C
C Otherwise, find the values of S and T, in the parametric equations
C for line segments AB and CD, for which intersection occurs.
C
        S=((XCPC-XCPA)*(YCPD-YCPC)-(XCPD-XCPC)*(YCPC-YCPA))/DNOM
        T=((XCPC-XCPA)*(YCPB-YCPA)-(XCPB-XCPA)*(YCPC-YCPA))/DNOM
C
C If both S and T are strictly between 0 and 1, the line segments
C intersect; otherwise, they don't.
C
        IF (S.GT.0..AND.S.LT.1..AND.T.GT.0..AND.T.LT.1.) ILSINT=1
C
C Done.
C
        RETURN
C
      END
