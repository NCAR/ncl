c
c $Id: termsdp.f,v 1.2 2003-08-11 22:44:03 haley Exp $
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
C make the same changes to the corresponding single precision routine.
C
      SUBROUTINE TERMSDP(DIAG,SDIAG,SIGMA,DEL)
      DOUBLE PRECISION SIGDEL
      DOUBLE PRECISION SINHM
      DOUBLE PRECISION COSHM
      DOUBLE PRECISION DENOM
c
      DOUBLE PRECISION DIAG,SDIAG,SIGMA,DEL
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
      IF (SIGMA.NE.0.D0) GO TO 1
      DIAG = DEL/3.D0
      SDIAG = DEL/6.D0
      RETURN
    1 SIGDEL = SIGMA*DEL
      CALL SNHCSHDP(SINHM,COSHM,SIGDEL,0)
      DENOM = SIGMA*SIGDEL* (1.D0+SINHM)
      DIAG = (COSHM-SINHM)/DENOM
      SDIAG = SINHM/DENOM
      RETURN
      END
