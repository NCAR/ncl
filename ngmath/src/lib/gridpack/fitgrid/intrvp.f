C
C $Id: intrvp.f,v 1.2 2000-07-13 02:49:23 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
