C
C $Id: intrvpdp.f,v 1.3 2008-07-27 03:10:11 haley Exp $
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
      FUNCTION INTRVPDP(T,X,N,P,TP)
      DOUBLE PRECISION TT
c
      INTEGER N
      DOUBLE PRECISION T,X(N),P,TP
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function determines the index of the interval
c (determined by a given increasing sequence) in which a
c given value lies, after translating the value to within
c the correct period.  it also returns this translated value.
c
c on input--
c
c   t is the given value.
c
c   x is a vector of strictly increasing values.
c
c   n is the length of x (n .ge. 2).
c
c and
c
c   p contains the period.
c
c on output--
c
c   tp contains a translated value of t (i. e. x(1) .le. tp,
c   tp .lt. x(1)+p, and tp = t + k*p for some integer k).
c
c   intrvldp returns an integer i such that
c
c          i = 1       if             tp .lt. x(2)  ,
c          i = n       if   x(n) .le. tp            ,
c          otherwise       x(i)  .le. tp .lt. x(i+1),
c
c none of the input parameters are altered.
c
c-----------------------------------------------------------
c
      SAVE I
      DATA I/1/
c
      NPER = (T-X(1))/P
      TP = T - DBLE(NPER)*P
      IF (TP.LT.X(1)) TP = TP + P
      TT = TP
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
              INTRVPDP = 1
              RETURN
          ELSE
              IL = 2
              IH = I
          END IF
      ELSE IF (TT.LE.X(I+1)) THEN
          INTRVPDP = I
          RETURN
      ELSE IF (TT.GE.X(N)) THEN
          I = N
          INTRVPDP = N
          RETURN
      ELSE
          IL = I + 1
          IH = N
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
          INTRVPDP = I
          RETURN
      END IF
      GO TO 1
      END
