C
C	$Id: msntvl.f,v 1.2 2000-07-12 16:26:21 haley Exp $
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
      FUNCTION MSNTVL (T,X,N)
C
      INTEGER N
      REAL T,X(N)
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
C THIS FUNCTION DETERMINES THE INDEX OF THE INTERVAL
C (DETERMINED BY A GIVEN INCREASING SEQUENCE) IN WHICH
C A GIVEN VALUE LIES.
C
C ON INPUT--
C
C   T IS THE GIVEN VALUE.
C
C   X IS A VECTOR OF STRICTLY INCREASING VALUES.
C
C AND
C
C   N IS THE LENGTH OF X (N .GE. 2).
C
C ON OUTPUT--
C
C   MSNTVL RETURNS AN INTEGER I SUCH THAT
C
C          I = 1       IF             T .LT. X(2)  ,
C          I = N-1     IF X(N-1) .LE. T            ,
C          OTHERWISE       X(I)  .LE. T .LT. X(I+1),
C
C NONE OF THE INPUT PARAMETERS ARE ALTERED.
C
C-----------------------------------------------------------
C
      SAVE I
      DATA I /1/
C
      TT = T
C
C CHECK FOR ILLEGAL I
C
      IF (I .GE. N) I = N/2
C
C CHECK OLD INTERVAL AND EXTREMES
C
      IF (TT .LT. X(I)) THEN
        IF (TT .LE. X(2)) THEN
          I = 1
          MSNTVL = 1
          RETURN
        ELSE
          IL = 2
          IH = I
        END IF
      ELSE IF (TT .LE. X(I+1)) THEN
        MSNTVL = I
        RETURN
      ELSE IF (TT .GE. X(N-1)) THEN
        I = N-1
        MSNTVL = N-1
        RETURN
      ELSE
        IL = I+1
        IH = N-1
      END IF
C
C BINARY SEARCH LOOP
C
    1 I = (IL+IH)/2
      IF (TT .LT. X(I)) THEN
        IH = I
      ELSE IF (TT .GT. X(I+1)) THEN
        IL = I+1
      ELSE
        MSNTVL = I
        RETURN
      END IF
      GO TO 1
      END
