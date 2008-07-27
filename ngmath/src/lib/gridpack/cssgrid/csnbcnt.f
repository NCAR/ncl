C
C	$Id: csnbcnt.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION CSNBCNT (LPL,LPTR)
      INTEGER LPL, LPTR(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/15/96
C
C   This function returns the number of neighbors of a node
C N0 in a triangulation created by Subroutine CSTRMESH.
C
C   This function is identical to the similarly named
C function in TRIPACK.
C
C
C On input:
C
C       LPL = LIST pointer to the last neighbor of N0 --
C             LPL = LEND(N0).
C
C       LPTR = Array of pointers associated with LIST.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       CSNBCNT = Number of neighbors of N0.
C
C Modules required by CSNBCNT:  None
C
C***********************************************************
C
      INTEGER K, LP
C
C Local parameters:
C
C K =  Counter for computing the number of neighbors
C LP = LIST pointer
C
      LP = LPL
      K = 1
C
    1 LP = LPTR(LP)
        IF (LP .EQ. LPL) GO TO 2
        K = K + 1
        GO TO 1
C
    2 CSNBCNT = K
      RETURN
      END
