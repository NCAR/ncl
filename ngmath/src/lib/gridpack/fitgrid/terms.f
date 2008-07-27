c
c $Id: terms.f,v 1.5 2008-07-27 03:10:12 haley Exp $
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
      subroutine terms (diag,sdiag,sigma,del)
c
      real diag,sdiag,sigma,del
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine computes the diagonal and superdiagonal
c terms of the tridiagonal linear system associated with
c spline under tension interpolation.
c
c on input--
c
c   sigma contains the tension factor.
c
c and
c
c   del contains the step size.
c
c on output--
c
c                sigma*del*cosh(sigma*del) - sinh(sigma*del)
c   diag = del*--------------------------------------------.
c                     (sigma*del)**2 * sinh(sigma*del)
c
c                   sinh(sigma*del) - sigma*del
c   sdiag = del*----------------------------------.
c                (sigma*del)**2 * sinh(sigma*del)
c
c and
c
c   sigma and del are unaltered.
c
c this subroutine references package module snhcsh.
c
c-----------------------------------------------------------
c
      if (sigma .ne. 0.) go to 1
      diag = del/3.
      sdiag = del/6.
      return
    1 sigdel = sigma*del
      call snhcsh (sinhm,coshm,sigdel,0)
      denom = sigma*sigdel*(1.+sinhm)
      diag = (coshm-sinhm)/denom
      sdiag = sinhm/denom
      return
      end
