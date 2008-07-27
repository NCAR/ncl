C
C	$Id: csintadd.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSINTADD (KK,I1,I2,I3, LIST,LPTR,LEND,LNEW )
      INTEGER KK, I1, I2, I3, LIST(*), LPTR(*), LEND(*),
     .        LNEW
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
C   This subroutine adds an interior node to a triangulation
C of a set of points on the unit sphere.  The data structure
C is updated with the insertion of node KK into the triangle
C whose vertices are I1, I2, and I3.  No optimization of the
C triangulation is performed.
C
C   This routine is identical to the similarly named routine
C in TRIPACK.
C
C
C On input:
C
C       KK = Index of the node to be inserted.  KK .GE. 1
C            and KK must not be equal to I1, I2, or I3.
C
C       I1,I2,I3 = Indexes of the counterclockwise-ordered
C                  sequence of vertices of a triangle which
C                  contains node KK.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.  Refer to Sub-
C                             routine CSTRMESH.  Triangle
C                             (I1,I2,I3) must be included
C                             in the triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node KK.  KK
C                             will be connected to nodes I1,
C                             I2, and I3.
C
C Modules required by CSINTADD:  CSINSERT, CSLSTPTR
C
C***********************************************************
C
      INTEGER CSLSTPTR
      INTEGER K, LP, N1, N2, N3
C
C Local parameters:
C
C K =        Local copy of KK
C LP =       LIST pointer
C N1,N2,N3 = Local copies of I1, I2, and I3
C
      K = KK
C
C Initialization.
C
      N1 = I1
      N2 = I2
      N3 = I3
C
C Add K as a neighbor of I1, I2, and I3.
C
      LP = CSLSTPTR(LEND(N1),N2,LIST,LPTR)
      CALL CSINSERT (K,LP, LIST,LPTR,LNEW )
      LP = CSLSTPTR(LEND(N2),N3,LIST,LPTR)
      CALL CSINSERT (K,LP, LIST,LPTR,LNEW )
      LP = CSLSTPTR(LEND(N3),N1,LIST,LPTR)
      CALL CSINSERT (K,LP, LIST,LPTR,LNEW )
C
C Add I1, I2, and I3 as neighbors of K.
C
      LIST(LNEW) = N1
      LIST(LNEW+1) = N2
      LIST(LNEW+2) = N3
      LPTR(LNEW) = LNEW + 1
      LPTR(LNEW+1) = LNEW + 2
      LPTR(LNEW+2) = LNEW
      LEND(K) = LNEW + 2
      LNEW = LNEW + 3
      RETURN
      END
