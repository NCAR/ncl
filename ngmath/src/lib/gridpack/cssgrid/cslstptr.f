C
C	$Id: cslstptr.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION CSLSTPTR (LPL,NB,LIST,LPTR)
      INTEGER LPL, NB, LIST(*), LPTR(*)
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
C   This function returns the index (LIST pointer) of NB in
C the adjacency list for N0, where LPL = LEND(N0).
C
C   This function is identical to the similarly named
C function in TRIPACK.
C
C
C On input:
C
C       LPL = LEND(N0)
C
C       NB = Index of the node whose pointer is to be re-
C            turned.  NB must be connected to N0.
C
C       LIST,LPTR = Data structure defining the triangula-
C                   tion.  Refer to Subroutine CSTRMESH.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       CSLSTPTR = Pointer such that LIST(CSLSTPTR) = NB or
C                LIST(CSLSTPTR) = -NB, unless NB is not a
C                neighbor of N0, in which case CSLSTPTR = LPL.
C
C Modules required by CSLSTPTR:  None
C
C***********************************************************
C
      INTEGER LP, ND
C
C Local parameters:
C
C LP = LIST pointer
C ND = Nodal index
C
      LP = LPTR(LPL)
    1 ND = LIST(LP)
        IF (ND .EQ. NB) GO TO 2
        LP = LPTR(LP)
        IF (LP .NE. LPL) GO TO 1
C
    2 CSLSTPTR = LP
      RETURN
      END
