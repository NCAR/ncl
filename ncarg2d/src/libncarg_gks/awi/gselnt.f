C
C	$Id: gselnt.f,v 1.4 2000-07-12 16:39:56 haley Exp $
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
      SUBROUTINE GSELNT (TNR)
C
C  SELECT NORMALIZATION TRANSFORMATION
C
      INTEGER ESELNT
      PARAMETER (ESELNT=52)
C
      include 'gkscom.h'
C
      INTEGER TNR
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESELNT,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the normalization transformation number is valid.
C
      IF (TNR.LT.0 .OR. TNR.GT.MNT) THEN
        ERS = 1
        CALL GERHND(50,ESELNT,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current normalization transformation variable in
C  the GKS state list.
C
      CNT = TNR
C
C  Reestablish character height and up vector, and
C  pattern size and reference point, and clipping rectangle.
C
      CALL GSCHH(CCHH)
      CALL GSCHUP(CCHUP(1),CCHUP(2))
C
C  Pattern size and pattern reference point currently not implemented.
C
C     CALL GSPA(CPA(1),CPA(2))
C     CALL GSPARF(CPARF(1),CPARF(2))
      CALL GSCLIP(CCLIP)
C
      RETURN
      END
