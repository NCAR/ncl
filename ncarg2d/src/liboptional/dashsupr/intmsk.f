C
C	$Id: intmsk.f,v 1.4 2008-07-27 00:23:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
