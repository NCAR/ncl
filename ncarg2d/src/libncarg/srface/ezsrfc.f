C
C	$Id: ezsrfc.f,v 1.2 2000-07-12 16:25:52 haley Exp $
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
      SUBROUTINE EZSRFC (Z,M,N,ANGH,ANGV,WORK)
      DIMENSION       Z(M,N)     ,WORK(1)
C
C                      WORK(2*M*N+M+N)
C
C PERSPECTIVE PICTURE OF A SURFACE STORED IN A TWO DIMENSIONAL ARRAY
C VIA A VERY SHORT ARGUMENT LIST.
C
C ASSUMPTIONS--
C     THE ENTIRE ARRAY IS TO BE DRAWN,
C     THE DATA IS EQUALLY SPACED (IN THE X-Y PLANE),
C     NO STEREO PAIRS.
C IF THESE ASSUMPTIONS ARE NOT MET USE SRFACE.
C
C ARGUMENTS--
C     Z    THE 2 DIMENSIONAL ARRAY TO BE DRAWN.
C     M    THE FIRST DIMENSION OF Z.
C     N    THE SECOND DIMENSION OF Z.
C     ANGH ANGLE IN DEGREES IN THE X-Y PLANE TO THE LINE OF SIGHT
C          (COUNTER-CLOCK WISE FROM THE PLUS-X AXIS).
C     ANGV ANGLE IN DEGREES FROM THE X-Y PLANE TO THE LINE OF SIGHT
C          (POSITIVE ANGLES ARE ABOVE THE MIDDLE Z, NEGATIVE BELOW).
C     WORK A SCRATCH STORAGE DIMENSIONED AT LEAST 2*M*N+M+N.
C
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                NOFFP      ,NSPVAL     ,SPV        ,BIGEST
      DIMENSION       S(6)
      DATA S(4),S(5),S(6)/0.0,0.0,0.0/
C
C  FACT1 IS THE PERSPECTIVE RATIO AND IS DEFINED TO BE THE RATIO
C         MAXIMUM(LENGTH,WIDTH)/HEIGHT
C
C  FACT2 IS THE RATIO   (LENGTH OF LINE OF SIGHT)/MAXIMUM(LENGTH,WIDTH)
C
      DATA FACT1,FACT2/2.0,5.0/
      BIGEST = R1MACH(2)
C
C FIND RANGE OF Z
C
      MX = M
      NY = N
      ANG1 = ANGH*3.14159265358979/180.
      ANG2 = ANGV*3.14159265358979/180.
      FLO = BIGEST
      HI = -FLO
      DO  20 J=1,NY
         DO  10 I=1,MX
            IF (NOFFP.EQ.1 .AND. Z(I,J).EQ.SPV) GO TO  10
            HI = AMAX1(Z(I,J),HI)
            FLO = AMIN1(Z(I,J),FLO)
   10    CONTINUE
   20 CONTINUE
C
C SET UP LINEAR X AND Y ARRAYS FOR SRFACE
C
      DELTA = (HI-FLO)/(AMAX0(MX,NY)-1.)*FACT1
      XMIN = -(FLOAT(MX/2)*DELTA+FLOAT(MOD(MX+1,2))*DELTA)
      YMIN = -(FLOAT(NY/2)*DELTA+FLOAT(MOD(NY+1,2))*DELTA)
      DO  30 I=1,MX
         WORK(I) = XMIN+FLOAT(I-1)*DELTA
   30 CONTINUE
      DO  40 J=1,NY
         K = MX+J
         WORK(K) = YMIN+FLOAT(J-1)*DELTA
   40 CONTINUE
C
C SET UP EYE POSITION
C
      FACTE = (HI-FLO)*FACT1*FACT2
      CANG2 = COS(ANG2)
      S(1) = FACTE*CANG2*COS(ANG1)
      S(2) = FACTE*CANG2*SIN(ANG1)
      S(3) = FACTE*SIN(ANG2)+(FLO+HI)*.5
C
C READY
C
      CALL SRFACE (WORK(1),WORK(MX+1),Z,WORK(K+1),MX,MX,NY,S,0.)
      RETURN
      END
