C
C	$Id: isinit.f,v 1.3 2000-08-22 15:04:55 haley Exp $
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
C The subroutine ISINIT.
C --- ---------- -------
C
      SUBROUTINE ISINIT
C
C Initialize machine-dependent constants.
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
C Determine how many 2-bit flags to put in each word of the screen
C models.
C
      NFPW=NX/LX
C
C Make sure that the value of NFPW is not too large.
C
      IF (NFPW.GT.I1MACH(5)/2) THEN
        CALL SETER ('ISINIT - INSTALLATION OF ISOSRF IMPROPERLY DONE - S
     +EE DIRECTIONS IN CODE',1,2)
        STOP
      END IF
C
C Compute the values of RNX and RNY, which are used to change fractional
C coordinates into indices on a screen model.
C
      RNX=REAL(NX-1)
      RNY=REAL(NY-1)
C
C Get the biggest real number.
C
      BIG = R1MACH(2)
C
C Generate a bit pattern containing 2*NFPW 1's.
C
      IONES=0
C
      DO 101 I=1,NFPW
        IONES=IOR(ISHIFT(IONES,2),3)
  101 CONTINUE
C
C Done.
C
      RETURN
C
      END
