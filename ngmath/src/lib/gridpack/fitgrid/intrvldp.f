C
C $Id: intrvldp.f,v 1.3 2008-07-27 03:10:11 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C NOTE: If you make any changes to this software, please remember to
C make the same changes to the corresponding single precision routine.
C
      FUNCTION INTRVLDP(T,X,N)
      DOUBLE PRECISION TT
c
      INTEGER N
      DOUBLE PRECISION T,X(N)
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function determines the index of the interval
c (determined by a given increasing sequence) in which
c a given value lies.
c
c on input--
c
c   t is the given value.
c
c   x is a vector of strictly increasing values.
c
c and
c
c   n is the length of x (n .ge. 2).
c
c on output--
c
c   intrvldp returns an integer i such that
c
c          i =  1       if             t .le. x(2)  ,
c          i =  n-1     if x(n-1) .le. t            ,
c          otherwise       x(i)  .le. t .le. x(i+1),
c
c none of the input parameters are altered.
c
c-----------------------------------------------------------
c
      SAVE I
      DATA I/1/
c
      TT = T
c
c check for illegal i
c
      IF (I.GE.N) I = N/2
c
c check old interval and extremes
c
      IF (TT.LT.X(I)) THEN
          IF (TT.LE.X(2)) THEN
              I = 1
              INTRVLDP = 1
              RETURN
          ELSE
              IL = 2
              IH = I
          END IF
      ELSE IF (TT.LE.X(I+1)) THEN
          INTRVLDP = I
          RETURN
      ELSE IF (TT.GE.X(N-1)) THEN
          I = N - 1
          INTRVLDP = N - 1
          RETURN
      ELSE
          IL = I + 1
          IH = N - 1
      END IF
c
c binary search loop
c
    1 I = (IL+IH)/2
      IF (TT.LT.X(I)) THEN
          IH = I
      ELSE IF (TT.GT.X(I+1)) THEN
          IL = I + 1
      ELSE
          INTRVLDP = I
          RETURN
      END IF
      GO TO 1
      END
