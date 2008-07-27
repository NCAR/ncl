C
C	$Id: csinsert.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSINSERT (K,LP, LIST,LPTR,LNEW )
      INTEGER K, LP, LIST(*), LPTR(*), LNEW
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/17/96
C
C   This subroutine inserts K as a neighbor of N1 following
C N2, where LP is the LIST pointer of N2 as a neighbor of
C N1.  Note that, if N2 is the last neighbor of N1, K will
C become the first neighbor (even if N1 is a boundary node).
C
C   This routine is identical to the similarly named routine
C in TRIPACK.
C
C
C On input:
C
C       K = Index of the node to be inserted.
C
C       LP = LIST pointer of N2 as a neighbor of N1.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LNEW = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        CSTRMESH.
C
C On output:
C
C       LIST,LPTR,LNEW = Data structure updated with the
C                        addition of node K.
C
C Modules required by CSINSERT:  None
C
C***********************************************************
C
      INTEGER LSAV
C
      LSAV = LPTR(LP)
      LPTR(LP) = LNEW
      LIST(LNEW) = K
      LPTR(LNEW) = LSAV
      LNEW = LNEW + 1
      RETURN
      END
