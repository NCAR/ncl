C
C	$Id: intmsk.f,v 1.2 2000-07-12 17:23:59 haley Exp $
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
      SUBROUTINE INTMSK(MASK,MASK3)
C
C     THIS SUBROUTINE INITIALIZES THE MASKS USED BY THE MODEL PICTURE
C
      DIMENSION MASK(*),MASK3(*)
      SAVE
C
C     GET NUMBER OF BITS PER WORD
C
      NBPW = I1MACH(5)
C
C     FILL MASKS
C
      IP1 = 1
      IP2 = 7
      MASK(1)  = 1
      MASK3(1) = 3
C
      DO  5 I=2,64
      MASK(I)  = 0
      MASK3(I) = 0
    5 CONTINUE
C
      DO 10 I=2,NBPW-1
      MASK(I)=ISHIFT(IP1,I-1)
      MASK3(I)=ISHIFT(IP2,I-2)
   10 CONTINUE
      MASK(NBPW)=ISHIFT(IP1,NBPW-1)
      MASK3(NBPW)=ISHIFT(MASK3(1),NBPW-2)
      RETURN
      END
