C
C	$Id: cstrlist.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSTRLIST (N,LIST,LPTR,LEND,NROW, NT,LTRI,IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), NROW, NT,
     .        LTRI(NROW,*), IER
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/20/96
C
C   This subroutine converts a triangulation data structure
C from the linked list created by Subroutine CSTRMESH to a
C triangle list.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       LIST,LPTR,LEND = Linked list data structure defin-
C                        ing the triangulation.  Refer to
C                        Subroutine CSTRMESH.
C
C       NROW = Number of rows (entries per triangle) re-
C              served for the triangle list LTRI.  The value
C              must be 6 if only the vertex indexes and
C              neighboring triangle indexes are to be
C              stored, or 9 if arc indexes are also to be
C              assigned and stored.  Refer to LTRI.
C
C The above parameters are not altered by this routine.
C
C       LTRI = Integer array of length at least NROW*NT,
C              where NT is at most 2N-4.  (A sufficient
C              length is 12N if NROW=6 or 18N if NROW=9.)
C
C On output:
C
C       NT = Number of triangles in the triangulation unless
C            IER .NE. 0, in which case NT = 0.  NT = 2N-NB-2
C            if NB .GE. 3 or 2N-4 if NB = 0, where NB is the
C            number of boundary nodes.
C
C       LTRI = NROW by NT array whose J-th column contains
C              the vertex nodal indexes (first three rows),
C              neighboring triangle indexes (second three
C              rows), and, if NROW = 9, arc indexes (last
C              three rows) associated with triangle J for
C              J = 1,...,NT.  The vertices are ordered
C              counterclockwise with the first vertex taken
C              to be the one with smallest index.  Thus,
C              LTRI(2,J) and LTRI(3,J) are larger than
C              LTRI(1,J) and index adjacent neighbors of
C              node LTRI(1,J).  For I = 1,2,3, LTRI(I+3,J)
C              and LTRI(I+6,J) index the triangle and arc,
C              respectively, which are opposite (not shared
C              by) node LTRI(I,J), with LTRI(I+3,J) = 0 if
C              LTRI(I+6,J) indexes a boundary arc.  Vertex
C              indexes range from 1 to N, triangle indexes
C              from 0 to NT, and, if included, arc indexes
C              from 1 to NA, where NA = 3N-NB-3 if NB .GE. 3
C              or 3N-6 if NB = 0.  The triangles are or-
C              dered on first (smallest) vertex indexes.
C
C       IER = Error indicator.
C             IER = 0 if no errors were encountered.
C             IER = 1 if N or NROW is outside its valid
C                     range on input.
C             IER = 2 if the triangulation data structure
C                     (LIST,LPTR,LEND) is invalid.  Note,
C                     however, that these arrays are not
C                     completely tested for validity.
C
C Modules required by CSTRLIST:  None
C
C Intrinsic function called by CSTRLIST:  ABS
C
C***********************************************************
C
      INTEGER I, I1, I2, I3, ISV, J, KA, KN, KT, LP, LP2,
     .        LPL, LPLN1, N1, N2, N3, NM2
      LOGICAL ARCS
C
C Local parameters:
C
C ARCS =     Logical variable with value TRUE iff are
C              indexes are to be stored
C I,J =      LTRI row indexes (1 to 3) associated with
C              triangles KT and KN, respectively
C I1,I2,I3 = Nodal indexes of triangle KN
C ISV =      Variable used to permute indexes I1,I2,I3
C KA =       Arc index and number of currently stored arcs
C KN =       Index of the triangle that shares arc I1-I2
C              with KT
C KT =       Triangle index and number of currently stored
C              triangles
C LP =       LIST pointer
C LP2 =      Pointer to N2 as a neighbor of N1
C LPL =      Pointer to the last neighbor of I1
C LPLN1 =    Pointer to the last neighbor of N1
C N1,N2,N3 = Nodal indexes of triangle KT
C NM2 =      N-2
C
C
C Test for invalid input parameters.
C
      IF (N .LT. 3  .OR.  (NROW .NE. 6  .AND.  NROW .NE. 9))
     .  GO TO 11
C
C Initialize parameters for loop on triangles KT = (N1,N2,
C   N3), where N1 < N2 and N1 < N3.
C
C   ARCS = TRUE iff arc indexes are to be stored.
C   KA,KT = Numbers of currently stored arcs and triangles.
C   NM2 = Upper bound on candidates for N1.
C
      ARCS = NROW .EQ. 9
      KA = 0
      KT = 0
      NM2 = N-2
C
C Loop on nodes N1.
C
      DO 9 N1 = 1,NM2
C
C Loop on pairs of adjacent neighbors (N2,N3).  LPLN1 points
C   to the last neighbor of N1, and LP2 points to N2.
C
        LPLN1 = LEND(N1)
        LP2 = LPLN1
    1     LP2 = LPTR(LP2)
          N2 = LIST(LP2)
          LP = LPTR(LP2)
          N3 = ABS(LIST(LP))
          IF (N2 .LT. N1  .OR.  N3 .LT. N1) GO TO 8
C
C Add a new triangle KT = (N1,N2,N3).
C
          KT = KT + 1
          LTRI(1,KT) = N1
          LTRI(2,KT) = N2
          LTRI(3,KT) = N3
C
C Loop on triangle sides (I2,I1) with neighboring triangles
C   KN = (I1,I2,I3).
C
          DO 7 I = 1,3
            IF (I .EQ. 1) THEN
              I1 = N3
              I2 = N2
            ELSEIF (I .EQ. 2) THEN
              I1 = N1
              I2 = N3
            ELSE
              I1 = N2
              I2 = N1
            ENDIF
C
C Set I3 to the neighbor of I1 that follows I2 unless
C   I2->I1 is a boundary arc.
C
            LPL = LEND(I1)
            LP = LPTR(LPL)
    2       IF (LIST(LP) .EQ. I2) GO TO 3
              LP = LPTR(LP)
              IF (LP .NE. LPL) GO TO 2
C
C   I2 is the last neighbor of I1 unless the data structure
C     is invalid.  Bypass the search for a neighboring
C     triangle if I2->I1 is a boundary arc.
C
            IF (ABS(LIST(LP)) .NE. I2) GO TO 12
            KN = 0
            IF (LIST(LP) .LT. 0) GO TO 6
C
C   I2->I1 is not a boundary arc, and LP points to I2 as
C     a neighbor of I1.
C
    3       LP = LPTR(LP)
            I3 = ABS(LIST(LP))
C
C Find J such that LTRI(J,KN) = I3 (not used if KN > KT),
C   and permute the vertex indexes of KN so that I1 is
C   smallest.
C
            IF (I1 .LT. I2  .AND.  I1 .LT. I3) THEN
              J = 3
            ELSEIF (I2 .LT. I3) THEN
              J = 2
              ISV = I1
              I1 = I2
              I2 = I3
              I3 = ISV
            ELSE
              J = 1
              ISV = I1
              I1 = I3
              I3 = I2
              I2 = ISV
            ENDIF
C
C Test for KN > KT (triangle index not yet assigned).
C
            IF (I1 .GT. N1) GO TO 7
C
C Find KN, if it exists, by searching the triangle list in
C   reverse order.
C
            DO 4 KN = KT-1,1,-1
              IF (LTRI(1,KN) .EQ. I1  .AND.  LTRI(2,KN) .EQ.
     .            I2  .AND.  LTRI(3,KN) .EQ. I3) GO TO 5
    4         CONTINUE
            GO TO 7
C
C Store KT as a neighbor of KN.
C
    5       LTRI(J+3,KN) = KT
C
C Store KN as a neighbor of KT, and add a new arc KA.
C
    6       LTRI(I+3,KT) = KN
            IF (ARCS) THEN
              KA = KA + 1
              LTRI(I+6,KT) = KA
              IF (KN .NE. 0) LTRI(J+6,KN) = KA
            ENDIF
    7       CONTINUE
C
C Bottom of loop on triangles.
C
    8     IF (LP2 .NE. LPLN1) GO TO 1
    9     CONTINUE
C
C No errors encountered.
C
      NT = KT
      IER = 0
      RETURN
C
C Invalid input parameter.
C
   11 NT = 0
      IER = 1
      RETURN
C
C Invalid triangulation data structure:  I1 is a neighbor of
C   I2, but I2 is not a neighbor of I1.
C
   12 NT = 0
      IER = 2
      RETURN
      END
