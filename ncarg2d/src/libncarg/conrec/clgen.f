C
C	$Id: clgen.f,v 1.4 2006-03-10 17:28:10 kennison Exp $
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
      SUBROUTINE CLGEN (Z,MX,NX,NNY,CCLO,CHI,CINC,NLA,NLM,CL,NCL,ICNST)
      SAVE
      DIMENSION       CL(NLM)    ,Z(MX,NNY)
      COMMON /CONRE1/ IOFFP      ,SPVAL
C
C CLGEN PUTS THE VALUES OF THE CONTOUR LEVELS IN CL.
C VARIABLE NAMES MATCH THOSE IN CONREC, WITH THE FOLLOWING ADDITIONS.
C         NCL     -NUMBER OF CONTOUR LEVELS PUT IN CL.
C         ICNST   -FLAG TO TELL CONREC IF A CONSTANT FIELD WAS DETECTED.
C                 .ICNST=0 MEANS NON-CONSTANT FIELD.
C                 .ICNST NON-ZERO MEANS CONSTANT FIELD.
C
C TO PRODUCE NON-UNIFORM CONTOUR LEVEL SPACING, REPLACE THE CODE IN THIS
C SUBROUTINE WITH CODE TO PRODUCE WHATEVER SPACING IS DESIRED.
C
      ICNST = 0
      NY = NNY
      CLO = CCLO
      GLO = CLO
      HA = CHI
      FANC = CINC
      CRAT = NLA
      IF (HA-GLO) 101,102,111
  101 GLO = HA
      HA = CLO
      GO TO 111
  102 IF (GLO .NE. 0.) GO TO 120
      GLO = Z(1,1)
      HA = Z(1,1)
      IF (IOFFP .EQ. 0) GO TO 107
      DO 106 J=1,NY
         DO 105 I=1,NX
            IF (Z(I,J) .EQ. SPVAL) GO TO 105
            GLO = Z(I,J)
            HA = Z(I,J)
            DO 104 JJ=J,NY
               DO 103 II=1,NX
                  IF (Z(II,JJ) .EQ. SPVAL) GO TO 103
                  GLO = MIN(Z(II,JJ),GLO)
                  HA = MAX(Z(II,JJ),HA)
  103          CONTINUE
  104       CONTINUE
            GO TO 110
  105    CONTINUE
  106 CONTINUE
      GO TO 110
  107 DO 109 J=1,NY
         DO 108 I=1,NX
            GLO = MIN(Z(I,J),GLO)
            HA = MAX(Z(I,J),HA)
  108    CONTINUE
  109 CONTINUE
  110 IF (GLO .GE. HA) GO TO 119
  111 IF (FANC) 112,113,114
  112 CRAT = MAX(1.,-FANC)
  113 FANC = (HA-GLO)/CRAT
      P = 10.**(INT(ALOG10(FANC)+5000.)-5000)
      FANC = REAL(INT(FANC/P))*P
  114 IF (CHI-CLO) 116,115,116
  115 GLO = REAL(INT(GLO/FANC))*FANC
      HA = REAL(INT(HA/FANC))*FANC*(1.+SIGN(1.E-6,HA))
  116 DO 117 K=1,NLM
         CC = GLO+REAL(K-1)*FANC
         IF (CC .GT. HA) GO TO 118
         KK = K
         CL(K) = CC
  117 CONTINUE
  118 NCL = KK
      CCLO = CL(1)
      CHI = CL(NCL)
      CINC = FANC
      RETURN
  119 ICNST = 1
      NCL = 1
      CCLO = GLO
      RETURN
  120 CL(1) = GLO
      NCL = 1
      RETURN
      END
