C
C	$Id: cssetup.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSSETUP (XI,YI,WI,WK,S1,S2,WT, ROW)
      DOUBLE PRECISION XI, YI, WI, WK, S1, S2, WT, ROW(6)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   05/09/92
C
C   This subroutine sets up the I-th row of an augmented
C regression matrix for a weighted least squares fit of a
C quadratic function Q(X,Y) to a set of data values Wi,
C where Q(0,0) = Wk.  The first 3 columns (quadratic terms)
C are scaled by 1/S2 and the fourth and fifth columns (lin-
C ear terms) are scaled by 1/S1.
C
C On input:
C
C       XI,YI = Coordinates of node I.
C
C       WI = Data value at node I.
C
C       WK = Data value interpolated by Q at the origin.
C
C       S1,S2 = Inverse scale factors.
C
C       WT = Weight factor corresponding to the I-th
C            equation.
C
C       ROW = Array of length 6.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       ROW = Array containing a row of the augmented re-
C             gression matrix.
C
C Modules required by CSSETUP:  None
C
C***********************************************************
C
      DOUBLE PRECISION W1, W2
C
C Local parameters:
C
C W1 = Weighted scale factor for the linear terms
C W2 = Weighted scale factor for the quadratic terms
C
      W1 = WT/S1
      W2 = WT/S2
      ROW(1) = XI*XI*W2
      ROW(2) = XI*YI*W2
      ROW(3) = YI*YI*W2
      ROW(4) = XI*W1
      ROW(5) = YI*W1
      ROW(6) = (WI-WK)*WT
      RETURN
      END
