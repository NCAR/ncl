C
C $Id: intrvl.f,v 1.4 2003-08-11 22:44:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
C NOTE: If you make any changes to this software, please remember to
C make the same changes to the corresponding double precision routine.
C
      function intrvl (t,x,n)
c
      integer n
      real t,x(n)
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
c   intrvl returns an integer i such that
c
c          i =  1       if             t .le. x(2)  ,
c          i =  n-1     if x(n-1) .le. t            ,
c          otherwise       x(i)  .le. t .le. x(i+1),
c
c none of the input parameters are altered.
c
c-----------------------------------------------------------
c
      save i
      data i /1/
c
      tt = t
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
          intrvl = 1
          return
        else
          il = 2
          ih = i
        end if
      else if (tt .le. x(i+1)) then
        intrvl = i
        return
      else if (tt .ge. x(n-1)) then
        i = n-1
        intrvl = n-1
        return
      else
        il = i+1
        ih = n-1
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
         intrvl = i
         return
      end if
      go to 1
      end
