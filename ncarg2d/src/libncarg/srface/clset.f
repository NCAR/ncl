C
C	$Id: clset.f,v 1.3 2000-08-22 15:06:26 haley Exp $
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
      SUBROUTINE CLSET (Z,MX,NX,NY,CHI,CLO,CINC,NLA,NLM,CL,NCL,ICNST,
     1                  IOFFP,SPVAL,BIGEST)
      DIMENSION       Z(MX,NY)   ,CL(NLM)
      DATA KK/0/
C
C CLSET PUTS THE VALUS OF THE CONTOUR LEVELS IN CL
C
      ICNST = 0
      GLO = CLO
      HA = CHI
      FANC = CINC
      CRAT = NLA
      IF (HA-GLO)  10, 20, 50
   10 GLO = HA
      HA = CLO
      GO TO  50
   20 GLO = BIGEST
      HA = -GLO
      DO  40 J=1,NY
         DO  30 I=1,NX
            IF (IOFFP.EQ.1 .AND. Z(I,J).EQ.SPVAL) GO TO  30
            GLO = AMIN1(Z(I,J),GLO)
            HA = AMAX1(Z(I,J),HA)
   30    CONTINUE
   40 CONTINUE
   50 IF (FANC)  60, 70, 90
   60 CRAT = -FANC
   70 FANC = (HA-GLO)/CRAT
      IF (FANC) 140,140, 80
   80 P = 10.**(IFIX(ALOG10(FANC)+500.)-500)
      FANC = AINT(FANC/P)*P
   90 IF (CHI-CLO) 110,100,110
  100 GLO = AINT(GLO/FANC)*FANC
      HA = AINT(HA/FANC)*FANC
  110 DO 120 K=1,NLM
         CC = GLO+FLOAT(K-1)*FANC
         IF (CC .GT. HA) GO TO 130
         KK = K
         CL(K) = CC
  120 CONTINUE
  130 NCL = KK
      RETURN
  140 ICNST = 1
      RETURN
      END
