C
C	$Id: isgfxy.f,v 1.3 2000-08-22 15:04:54 haley Exp $
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
C
C The subroutine ISGFXY.
C --- ---------- -------
C
      SUBROUTINE ISGFXY (X,Y,RMX,RMY)
C
C This routine, given two coordinates of a point on the current slab,
C supplies the missing third coordinate and computes the fractional
C coordinates of the projection of that point on the plotter frame.
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
C Arithmetic statement functions for scaling.
C
      SU(UTEMP) = UTEMP
      SV(VTEMP) = VTEMP
      SW(WTEMP) = WTEMP
C
      XX = X
      YY = Y
      IF (ISLBT) 10,20,30
   10 CALL ISTR32 (SU(U),SV(XX-1.),SW(YY-1.),RMX,RMY,DUM,2)
      GO TO  40
   20 CALL ISTR32 (SU(XX-1.),SV(V),SW(YY-1.),RMX,RMY,DUM,2)
      GO TO  40
   30 CALL ISTR32 (SU(XX-1.),SV(YY-1.),SW(W),RMX,RMY,DUM,2)
   40 RETURN
C
      END
