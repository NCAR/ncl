C
C $Id: ilsint.f,v 1.3 2000-08-22 15:07:05 haley Exp $
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
