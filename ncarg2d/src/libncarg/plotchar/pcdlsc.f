C
C $Id: pcdlsc.f,v 1.7 2000-07-12 16:24:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE PCDLSC (IFCI)
C
C The subroutine PCDLSC may be called by the user to define standard
C values for special colors to be used for the filled fonts.  It has
C only one argument, IFCI, which is the first color index to be used by
C the subroutine in defining the colors.  It may use up to sixteen
C color indices (IFCI, IFCI+1, IFCI+2, ... ).
C
C COMMON block declarations.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),IBNU,
     +                IBXC(3),IBXF,ICEN,IORD,IOUC,IOUF,IPCC,IQUF,
     +                ISHC,ISHF,ITEF,JCOD,LSCI(16),NFCC,NODF,RBXL,
     +                RBXM,RBXX,RBXY,ROLW,RPLW,RSLW,SHDX,SHDY,SIZA,
     +                SSIC,SSPR,SUBS,VPIC(3),WPIC(3),XBEG,XCEN,XEND,
     +                XMUL(3),YBEG,YCEN,YEND,YMUL(3),ZINX,ZINY,ZINZ
      SAVE   /PCPRMS/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PCDLSC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for error in the argument.
C
      IF (IFCI.LT.0) THEN
        CALL SETER ('PCDLSC - FIRST COLOR INDEX IS LESS THAN ZERO',2,1)
        RETURN
      END IF
C
C Define the colors and store the color indices in the array LSCI.  The
C code below is to be replaced at such time as it is decided what the
C special colors to be used really are.  This code is intended for test
C purposes; it just interpolates a set of colors between red and blue.
C
      DO 101 I=1,16
        LSCI(I)=IFCI-1+I
        CALL GSCR (1,IFCI-1+I,REAL(I-1)/15.,0.,REAL(16-I)/15.)
  101 CONTINUE
C
      RETURN
C
      END
