C
C	$Id: reord.f,v 1.5 2008-07-27 00:23:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE REORD (CL,NCL,C1,MARK,NMG)
      DIMENSION       CL(NCL)    ,C1(NCL)
      DIMENSION IEND(5)
      LOGICAL ENDIT,FLAG
      SAVE
C
C THIS ROUTINE PUTS THE MAJOR (LABELED) LEVELS IN THE BEGINNING OF CL
C AND THE MINOR (UNLABELED) LEVELS IN END OF CL.  THE NUMBER OF MAJOR
C LEVELS IS RETURNED IN MARK.  C1 IS USED AS A WORK SPACE.  NMG IS THE
C NUMBER OF MINOR GAPS (ONE MORE THAN THE NUMBER OF MINOR LEVELS BETWEEN
C MAJOR LEVELS).
C
      NL = NCL
      IF (NL.LE.4 .OR. NMG.LE.1) GO TO 120
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
      ENDIT = .FALSE.
C
C     START OF MINOR LEVEL REORDERING
C
  111 IC1 = MARK+1
      IBEG = 0
      DO 112 I=1,NMG
         IF (C1(1) .EQ. CL(I))  GO TO 113
  112 CONTINUE
  113 IEND(1) = I
      ND = 1
 1135 FLAG = .FALSE.
  114 MID = (IEND(ND)+IBEG+1)/2
      IF (MID .EQ. IEND(ND))  GO TO 115
      IEND(ND+1) = MID
      ND = ND+1
      FLAG = .TRUE.
      C1(IC1) = CL(MID)
      IC1 = IC1+1
      GO TO 114
  115 IF ( .NOT. FLAG)  ND = ND+1
      IBEG = IEND(ND-1)
      IF (ND .EQ. 2)  GO TO 117
      ND = ND-2
      GO TO 1135
  117 IEND(1) = IEND(1) + NMG
      IF (ENDIT)  GO TO 118
      ND = 1
      IF (IEND(1) .LE. NL) GO TO 1135
      IEND(1) = NL+1
      ENDIT = .TRUE.
      GO TO 1135
  118 DO 119 I=1,NL
         CL(I) = C1(I)
  119 CONTINUE
      RETURN
  120 MARK = NL
      RETURN
      END
