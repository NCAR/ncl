C
C $Id: ceez.f,v 1.5 2008-07-27 03:10:10 haley Exp $
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
      subroutine ceez (del1,del2,sigma,c1,c2,c3,n)
c
      real del1,del2,sigma,c1,c2,c3
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the coefficients c1, c2, and c3
c used to determine endpoint slopes. specifically, if
c function values y1, y2, and y3 are given at points x1, x2,
c and x3, respectively, the quantity c1*y1 + c2*y2 + c3*y3
c is the value of the derivative at x1 of a spline under
c tension (with tension factor sigma) passing through the
c three points and having third derivative equal to zero at
c x1. optionally, only two values, c1 and c2 are determined.
c
c on input--
c
c   del1 is x2-x1 (.gt. 0.).
c
c   del2 is x3-x1 (.gt. 0.). if n .eq. 2, this parameter is
c   ignored.
c
c   sigma is the tension factor.
c
c and
c
c   n is a switch indicating the number of coefficients to
c   be returned. if n .eq. 2 only two coefficients are
c   returned. otherwise all three are returned.
c
c on output--
c
c   c1, c2, and c3 contain the coefficients.
c
c none of the input parameters are altered.
c
c this subroutine references package module snhcsh.
c
c-----------------------------------------------------------
c
      if (n .eq. 2) go to 2
      if (sigma .ne. 0.) go to 1
      del = del2-del1
c
c tension .eq. 0.
c
      c1 = -(del1+del2)/(del1*del2)
      c2 = del2/(del1*del)
      c3 = -del1/(del2*del)
      return
c
c tension .ne. 0.
c
    1 call snhcsh (dummy,coshm1,sigma*del1,1)
      call snhcsh (dummy,coshm2,sigma*del2,1)
      delp = sigma*(del2+del1)/2.
      delm = sigma*(del2-del1)/2.
      call snhcsh (sinhmp,dummy,delp,-1)
      call snhcsh (sinhmm,dummy,delm,-1)
      denom = coshm1*(del2-del1)-2.*del1*delp*delm*
     *        (1.+sinhmp)*(1.+sinhmm)
      c1 = 2.*delp*delm*(1.+sinhmp)*(1.+sinhmm)/denom
      c2 = -coshm2/denom
      c3 = coshm1/denom
      return
c
c two coefficients
c
    2 c1 = -1./del1
      c2 = -c1
      return
      end
