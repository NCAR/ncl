C
C	$Id: msceez.f,v 1.2 2000-07-12 16:26:21 haley Exp $
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
      SUBROUTINE MSCEEZ (DEL1,DEL2,SIGMA,C1,C2,C3,N)
C
C ---------------------------------------------------------------------
C Note:  This routine comes from a proprietary package called FITPACK.
C It is used in the NCAR graphics package by permission of the author,
C Alan Cline.
C ---------------------------------------------------------------------
C
C                                            CODED BY ALAN KAYLOR CLINE
C                                         FROM FITPACK -- JUNE 22, 1986
C                                   A CURVE AND SURFACE FITTING PACKAGE
C                                 A PRODUCT OF PLEASANT VALLEY SOFTWARE
C                             8603 ALTUS COVE, AUSTIN, TEXAS 78759, USA
C
C ---------------------------------------------------------------------
C
C THIS SUBROUTINE DETERMINES THE COEFFICIENTS C1, C2, AND C3
C USED TO DETERMINE ENDPOINT SLOPES. SPECIFICALLY, IF
C FUNCTION VALUES Y1, Y2, AND Y3 ARE GIVEN AT POINTS X1, X2,
C AND X3, RESPECTIVELY, THE QUANTITY C1*Y1 + C2*Y2 + C3*Y3
C IS THE VALUE OF THE DERIVATIVE AT X1 OF A SPLINE UNDER
C TENSION (WITH TENSION FACTOR SIGMA) PASSING THROUGH THE
C THREE POINTS AND HAVING THIRD DERIVATIVE EQUAL TO ZERO AT
C X1. OPTIONALLY, ONLY TWO VALUES, C1 AND C2 ARE DETERMINED.
C
C ON INPUT--
C
C   DEL1 IS X2-X1 (.GT. 0.).
C
C   DEL2 IS X3-X1 (.GT. 0.). IF N .EQ. 2, THIS PARAMETER IS
C   IGNORED.
C
C   SIGMA IS THE TENSION FACTOR.
C
C   N IS A SWITCH INDICATING THE NUMBER OF COEFFICIENTS TO
C   BE RETURNED. IF N .EQ. 2 ONLY TWO COEFFICIENTS ARE
C   RETURNED. OTHERWISE ALL THREE ARE RETURNED.
C
C ON OUTPUT--
C
C   C1, C2, AND C3 CONTAIN THE COEFFICIENTS.
C
C NONE OF THE INPUT PARAMETERS ARE ALTERED.
C
C THIS SUBROUTINE REFERENCES PACKAGE MODULE MSSHCH.
C
C-----------------------------------------------------------
C
      IF (N .EQ. 2) GO TO 2
      IF (SIGMA .NE. 0.) GO TO 1
      DEL = DEL2-DEL1
C
C TENSION .EQ. 0.
C
      C1 = -(DEL1+DEL2)/(DEL1*DEL2)
      C2 = DEL2/(DEL1*DEL)
      C3 = -DEL1/(DEL2*DEL)
      RETURN
C
C TENSION .NE. 0.
C
    1 CALL MSSHCH (DUMMY,COSHM1,SIGMA*DEL1,1)
      CALL MSSHCH (DUMMY,COSHM2,SIGMA*DEL2,1)
      DELP = SIGMA*(DEL2+DEL1)/2.
      DELM = SIGMA*(DEL2-DEL1)/2.
      CALL MSSHCH (SINHMP,DUMMY,DELP,-1)
      CALL MSSHCH (SINHMM,DUMMY,DELM,-1)
      DENOM = COSHM1*(DEL2-DEL1)-2.*DEL1*DELP*DELM*
     *        (1.+SINHMP)*(1.+SINHMM)
      C1 = 2.*DELP*DELM*(1.+SINHMP)*(1.+SINHMM)/DENOM
      C2 = -COSHM2/DENOM
      C3 = COSHM1/DENOM
      RETURN
C
C TWO COEFFICIENTS
C
    2 C1 = -1./DEL1
      C2 = -C1
      RETURN
      END
