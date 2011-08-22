C
C $Id:$
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
      subroutine curvps (n,x,y,p,d,isw,s,eps,ys,ysp,sigma,
     *                   temp,ierr)
C
      integer n,isw,ierr
      real x(n),y(n),p,d(n),s,eps,ys(n),ysp(n),sigma,
     *     temp(n,11)
c
c                                 coded by alan kaylor cline
c                              from fitpack -- april 8, 1991
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a periodic smoothing spline under tension. for a
c given increasing sequence of abscissae (x(i)), i = 1,...,n
c and associated ordinates (y(i)), i = 1,...,n, letting p be
c the period, x(n+1) = x(1)+p, and y(n+1) = y(1), the
c function determined minimizes the summation from i = 1 to
c n of the square of the second derivative of f plus sigma
c squared times the difference of the first derivative of f
c and (f(x(i+1))-f(x(i)))/(x(i+1)-x(i)) squared, over all
c functions f with period p and two continuous derivatives
c such that the summation of the square of
c (f(x(i))-y(i))/d(i) is less than or equal to a given
c constant s, where (d(i)), i = 1,...,n are a given set of
c observation weights. the function determined is a periodic
c spline under tension with third derivative discontinuities
c at (x(i)) i = 1,...,n (and all periodic translations of
c these values). for actual computation of points on the
c curve it is necessary to call the function curvp2. the
c determination of the curve is performed by subroutine
c curvpp, the subroutin curvps only decomposes the workspace
c for curvpp.
c
c on input--
c
c   n is the number of values to be smoothed (n.ge.2).
c
c   x is an array of the n increasing abscissae of the
c   values to be smoothed.
c
c   y is an array of the n ordinates of the values to be
c   smoothed, (i. e. y(k) is the functional value
c   corresponding to x(k) ).
c
c   p is the period (p .gt. x(n)-x(1)).
c
c   d is a parameter containing the observation weights.
c   this may either be an array of length n or a scalar
c   (interpreted as a constant). the value of d
c   corresponding to the observation (x(k),y(k)) should
c   be an approximation to the standard deviation of error.
c
c   isw contains a switch indicating whether the parameter
c   d is to be considered a vector or a scalar,
c          = 0 if d is an array of length n,
c          = 1 if d is a scalar.
c
c   s contains the value controlling the smoothing. this
c   must be non-negative. for s equal to zero, the
c   subroutine does interpolation, larger values lead to
c   smoother funtions. if parameter d contains standard
c   deviation estimates, a reasonable value for s is
c   float(n).
c
c   eps contains a tolerance on the relative precision to
c   which s is to be interpreted. this must be greater than
c   or equal to zero and less than or equal to one. a
c   reasonable value for eps is sqrt(2./float(n)).
c
c   ys is an array of length at least n.
c
c   ysp is an array of length at least n.
c
c   sigma contains the tension factor. this value indicates
c   the degree to which the first derivative part of the
c   smoothing functional is emphasized. if sigma is nearly
c   zero (e. g. .001) the resulting curve is approximately a
c   cubic spline. if sigma is large (e. g. 50.) the
c   resulting curve is nearly a polygonal line. if sigma
c   equals zero a cubic spline results. a standard value for
c   sigma is approximately 1.
c
c and
c
c   temp is an array of length at least 11*n which is used
c   for scratch storage.
c
c on output--
c
c   ys contains the smoothed ordinate values.
c
c   ysp contains the values of the second derivative of the
c   smoothed curve at the given nodes.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2,
c        = 2 if s is negative,
c        = 3 if eps is negative or greater than one,
c        = 4 if x-values are not strictly increasing,
c        = 5 if a d-value is non-positive,
c        = 6 if p is less than or equal to x(n)-x(1).
c
c and
c
c   n, x, y, p, d, isw, s, eps, and sigma are unaltered.
c
c this subroutine references package modules curvpp, ftstore,
c terms, and snhcsh.
c
c-----------------------------------------------------------
c
c decompose temp into eleven arrays and call curvpp
c
      call curvpp (n,x,y,p,d,isw,s,eps,ys,ysp,sigma,
     *             temp(1,1),temp(1,2),temp(1,3),temp(1,4),
     *             temp(1,5),temp(1,6),temp(1,7),temp(1,8),
     *             temp(1,9),temp(1,10),temp(1,11),ierr)
      return
      end
