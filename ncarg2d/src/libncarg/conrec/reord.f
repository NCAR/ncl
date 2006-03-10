C
C	$Id: reord.f,v 1.4 2006-03-10 17:28:10 kennison Exp $
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
      SUBROUTINE REORD (CL,NCL,C1,MARK,NMG)
      SAVE
      DIMENSION       CL(NCL)    ,C1(NCL)
C
C THIS ROUTINE PUTS THE MAJOR (LABELED) LEVELS IN THE BEGINNING OF CL
C AND THE MINOR (UNLABELED) LEVELS IN END OF CL.  THE NUMBER OF MAJOR
C LEVELS IS RETURNED IN MARK.  C1 IS USED AS A WORK SPACE.  NMG IS THE
C NUMBER OF MINOR GAPS (ONE MORE THAN THE NUMBER OF MINOR LEVELS BETWEEN
C MAJOR LEVELS).
C
      NL = NCL
      IF (NL.LE.4 .OR. NMG.LE.1) GO TO 113
      NML = NMG-1
      IF (NL .LE. 10) NML = 1
C
C CHECK FOR ZERO OR OTHER NICE NUMBER FOR A MAJOR LINE
C
      NMLP1 = NML+1
      DO 101 I=1,NL
         ISAVE = I
         IF (CL(I) .EQ. 0.) GO TO 104
  101 CONTINUE
      L = NL/2
      L = ALOG10(ABS(CL(L)))+1.
      Q = 10.**L
      DO 103 J=1,3
         Q = Q/10.
         DO 102 I=1,NL
            ISAVE = I
            IF (MOD(ABS(CL(I)+1.E-9*CL(I))/Q,REAL(NMLP1)) .LE. .0001)
     1          GO TO 104
  102    CONTINUE
  103 CONTINUE
      ISAVE = NL/2
C
C PUT MAJOR LEVELS IN C1
C
  104 ISTART = MOD(ISAVE,NMLP1)
      IF (ISTART .EQ. 0) ISTART = NMLP1
      NMAJL = 0
      DO 105 I=ISTART,NL,NMLP1
         NMAJL = NMAJL+1
         C1(NMAJL) = CL(I)
  105 CONTINUE
      MARK = NMAJL
      L = NMAJL
C
C PUT MINOR LEVELS IN C1
C
      IF (ISTART .EQ. 1) GO TO 107
      DO 106 I=2,ISTART
         ISUB = L+I-1
         C1(ISUB) = CL(I-1)
  106 CONTINUE
  107 L = NMAJL+ISTART-1
      DO 109 I=2,NMAJL
         DO 108 J=1,NML
            L = L+1
            ISUB = ISTART+(I-2)*NMLP1+J
            C1(L) = CL(ISUB)
  108    CONTINUE
  109 CONTINUE
      NLML = NL-L
      IF (L .EQ. NL) GO TO 111
      DO 110 I=1,NLML
         L = L+1
         C1(L) = CL(L)
  110 CONTINUE
C
C PUT REORDERED ARRAY BACK IN ORIGINAL PLACE
C
  111 DO 112 I=1,NL
         CL(I) = C1(I)
  112 CONTINUE
      RETURN
  113 MARK = NL
      RETURN
      END
