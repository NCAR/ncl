C
C $Id: ctpisb.f,v 1.1 2004-03-19 22:51:56 kennison Exp $
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
      SUBROUTINE CTPISB (XCPA,YCPA,XCPB,YCPB,XCPC,YCPC,XCPD,YCPD,
     +                                            XCOP,YCOP,IFLG)
C
C This function checks for overlap of the 2D line segments AB and CD;
C if they overlap, it adds the X and Y coordinates of the point of
C overlap to XCOP and YCOP and bumps the value of the counter IFLG.
C
C Compute a denominator needed below.  (Its value is zero if and only
C if the line segments are parallel.)
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
C If both S and T are between 0 and 1, the line segments intersect;
C otherwise, they don't.
C
      IF (S.GE.0..AND.S.LE.1..AND.T.GE.0..AND.T.LE.1.) THEN
        XCOP=XCOP+(XCPA+S*(XCPB-XCPA))
        YCOP=YCOP+(YCPA+S*(YCPB-YCPA))
        IFLG=IFLG+1
      END IF
C
C Done.
C
      RETURN
C
      END
