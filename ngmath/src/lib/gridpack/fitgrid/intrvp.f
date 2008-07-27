C
C $Id: intrvp.f,v 1.5 2008-07-27 03:10:11 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C NOTE: If you make any changes to this software, please remember to
C make the same changes to the corresponding double precision routine.
C
      function intrvp (t,x,n,p,tp)
c
      integer n
      real t,x(n),p,tp
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
c   intrvl returns an integer i such that
c
c          i = 1       if             tp .lt. x(2)  ,
c          i = n       if   x(n) .le. tp            ,
c          otherwise       x(i)  .le. tp .lt. x(i+1),
c
c none of the input parameters are altered.
c
c-----------------------------------------------------------
c
      save i
      data i /1/
c
      nper = (t-x(1))/p
      tp = t-float(nper)*p
      if (tp .lt. x(1)) tp = tp+p
      tt = tp
c
c check for illegal i
c
      if (i .ge. n) i = n/2
c
c check old interval and extremes
c
      if (tt .lt. x(i)) then
        if (tt .le. x(2)) then
          i = 1
          intrvp = 1
          return
        else
          il = 2
          ih = i
        end if
      else if (tt .le. x(i+1)) then
        intrvp = i
        return
      else if (tt .ge. x(n)) then
        i = n
        intrvp = n
        return
      else
        il = i+1
        ih = n
      end if
c
c binary search loop
c
    1 i = (il+ih)/2
      if (tt .lt. x(i)) then
         ih = i
      else if (tt .gt. x(i+1)) then
         il = i+1
      else
         intrvp = i
         return
      end if
      go to 1
      end
