C
C	$Id: gssgt.f,v 1.2 2000-07-12 16:39:59 haley Exp $
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
      SUBROUTINE GSSGT(SGNA,M)
C
C  SET SEGMENT TRANSFORMATION
C
      INTEGER ESSGT
      PARAMETER (ESSGT=64)
C
      include 'gkscom.h'
C
      INTEGER SGNA
      REAL    M(2,3)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,ESSGT,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the segment name is valid.
C
      IF (SGNA.LT.0 .OR. SGNA.GT.99) THEN
        ERS = 1
        CALL GERHND(120,ESSGT,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the segment exists.
C
      NXST = 0
      DO 200 I=1,NUMSEG
        IF (SEGS(I) .EQ. SGNA) THEN
          NXST = 1
          ISNDX = I
        ENDIF
  200 CONTINUE
      IF (NXST .EQ. 0) THEN
        ERS = 1
        CALL GERHND(122,ESSGT,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Define the segment transformation matrix for segment SGNA.
C
      SEGT(ISNDX,1,1) = M(1,1)
      SEGT(ISNDX,1,2) = M(1,2)
      SEGT(ISNDX,1,3) = M(1,3)
      SEGT(ISNDX,2,1) = M(2,1)
      SEGT(ISNDX,2,2) = M(2,2)
      SEGT(ISNDX,2,3) = M(2,3)
C
      RETURN
      END
