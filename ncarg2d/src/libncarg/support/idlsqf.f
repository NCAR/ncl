C
C $Id: idlsqf.f,v 1.3 2000-08-22 15:06:53 haley Exp $
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
      SUBROUTINE IDLSQF (X,Y,Z,N,A,B,C,XAVG,YAVG)
C
        DIMENSION X(N),Y(N),Z(N)
C
C IDLSQF fits a plane to the data defined by points (X(I),Y(I),Z(I)),
C for I from 1 to N.  It returns the coefficients A, B, and C in the
C equation "Z=AX+BY+C".  It also returns the average X and average Y.
C
        SA=0.
        SB=0.
        SC=0.
        SD=0.
        SE=0.
        SF=0.
        SG=0.
        SH=0.
C
        DO 101 I=1,N
          SA=SA+X(I)
          SB=SB+Y(I)
          SC=SC+Z(I)
          SD=SD+X(I)*X(I)
          SE=SE+Y(I)*Y(I)
          SF=SF+X(I)*Y(I)
          SG=SG+Y(I)*Z(I)
          SH=SH+X(I)*Z(I)
  101   CONTINUE
C
        RN=REAL(N)
C
        A=((RN*SH-SA*SC)*(RN*SE-SB*SB)-(RN*SF-SA*SB)*(RN*SG-SB*SC))/
     +    ((RN*SD-SA*SA)*(RN*SE-SB*SB)-(RN*SF-SA*SB)*(RN*SF-SA*SB))
        B=((RN*SD-SA*SA)*(RN*SG-SB*SC)-(RN*SF-SA*SB)*(RN*SH-SA*SC))/
     +    ((RN*SD-SA*SA)*(RN*SE-SB*SB)-(RN*SF-SA*SB)*(RN*SF-SA*SB))
        C=(SC-SA*A-SB*B)/RN
C
        XAVG=SA/RN
        YAVG=SB/RN
C
C Done.
C
        RETURN
C
      END
