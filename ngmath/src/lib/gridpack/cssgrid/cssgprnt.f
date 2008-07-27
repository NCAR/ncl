C
C	$Id: cssgprnt.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSSGPRNT (N,LUNIT,LIST,LPTR,LEND,SIGMA)
      INTEGER N, LUNIT, LIST(*), LPTR(*), LEND(N)
      DOUBLE PRECISION SIGMA(*)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/21/98
C
C   Given a triangulation of a set of nodes on the unit
C sphere, along with an array of tension factors associated
C with the triangulation arcs, this subroutine prints the
C list of arcs (with tension factors) ordered by endpoint
C nodal indexes.  An arc is identified with its smaller
C endpoint index:  N1-N2, where N1 < N2.
C
C   This routine is identical to the similarly named routine
C in SRFPACK.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  3 .LE. N
C           .LE. 9999.
C
C       LUNIT = Logical unit for output.  0 .LE. LUNIT .LE.
C               99.  Output is printed on unit 6 if LUNIT is
C               outside its valid range.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to STRIPACK
C                        Subroutine CSTRMESH.
C
C       SIGMA = Array of length 2*NA = 6*(N-1)-2*NB, where
C               NA and NB are the numbers of arcs and boun-
C               dary nodes, respectively, containing tension
C               factors associated with arcs in one-to-one
C               correspondence with LIST entries.  Note that
C               each arc N1-N2 has two LIST entries and
C               thus, SIGMA(I) and SIGMA(J) should be iden-
C               tical, where LIST(I) = N2 (in the adjacency
C               list for N1) and LIST(J) = N1 (in the list
C               associated with N2).  Both SIGMA(I) and
C               SIGMA(J) are printed if they are not iden-
C               tical.
C
C None of the parameters are altered by this routine.
C
C STRIPACK module required by CSSGPRNT:  CSLSTPTR
C
C Intrinsic function called by CSSGPRNT:  ABS
C
C***********************************************************
C
      INTEGER CSLSTPTR
      INTEGER LP1, LP2, LPL, LUN, N1, N2, NA, NAT, NB, NE,
     .        NL, NLMAX, NM1, NMAX
      LOGICAL ERROR
      DOUBLE PRECISION SIG
      DATA NMAX/9999/,  NLMAX/58/
C
      LUN = LUNIT
      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
C
C Print a heading, test for invalid N, and initialize coun-
C   ters:
C
C NL = Number of lines printed on the current page
C NA = Number of arcs encountered
C NE = Number of errors in SIGMA encountered
C NB = Number of boundary nodes encountered
C
      WRITE (LUN,100) N
      IF (N .LT. 3  .OR.  N .GT. NMAX) GO TO 4
      NL = 6
      NA = 0
      NE = 0
      NB = 0
C
C Outer loop on nodes N1.  LPL points to the last neighbor
C   of N1.
C
      NM1 = N - 1
      DO 3 N1 = 1,NM1
        LPL = LEND(N1)
        IF (LIST(LPL) .LT. 0) NB = NB + 1
        LP1 = LPL
C
C Inner loop on neighbors N2 of N1 such that N1 < N2.
C
    1   LP1 = LPTR(LP1)
          N2 = ABS(LIST(LP1))
          IF (N2 .LT. N1) GO TO 2
          NA = NA + 1
          SIG = SIGMA(LP1)
C
C   Test for an invalid SIGMA entry.
C
          LP2 = CSLSTPTR (LEND(N2),N1,LIST,LPTR)
          ERROR = SIGMA(LP2) .NE. SIG
          IF (ERROR) NE = NE + 1
C
C   Print a line and update the counters.
C
          IF (.NOT. ERROR) WRITE (LUN,110) N1, N2, SIG
          IF (ERROR) WRITE (LUN,120) N1, N2, SIG, SIGMA(LP2)
          NL = NL + 1
          IF (NL .GE. NLMAX) THEN
            WRITE (LUN,130)
            NL = 1
          ENDIF
C
C Bottom of loop on neighbors N2 of N1.
C
    2     IF (LP1 .NE. LPL) GO TO 1
    3   CONTINUE
      LPL = LEND(N)
      IF (LIST(LPL) .LT. 0) NB = NB + 1
C
C Test for errors in SIGMA.
C
      IF (NE .GT. 0) WRITE (LUN,200) NE
C
C Print NA and test for an invalid triangulation.
C
      WRITE (LUN,140) NA
      IF (NB .NE. 0) THEN
        NAT = 3*NM1 - NB
      ELSE
        NAT = 3*N - 6
      ENDIF
      IF (NAT .NE. NA) WRITE (LUN,210) NAT
      RETURN
C
C N is outside its valid range.
C
    4 WRITE (LUN,220) NMAX
      RETURN
C
C Print formats:
C
  100 FORMAT ('1',14X,'TENSION FACTORS,  N =',I5,
     .        ' NODES'//1X,18X,'N1',5X,'N2',8X,'TENSION'//)
  110 FORMAT (1X,16X,I4,3X,I4,5X,F12.8)
  120 FORMAT (1X,16X,I4,3X,I4,5X,F12.8,3X,F12.8,' *')
  130 FORMAT ('1')
  140 FORMAT (//1X,10X,'NA =',I5,' ARCS')
C
C Error messages:
C
  200 FORMAT (//1X,10X,'*',I5,' ERRORS IN SIGMA')
  210 FORMAT (/1X,10X,'*** ERROR IN TRIANGULATION -- ',
     .        '3N-NB-3 = ',I5,' ***')
  220 FORMAT (1X,10X,'*** N IS OUT OF RANGE -- NMAX = ',
     .        I4,' ***')
      END
