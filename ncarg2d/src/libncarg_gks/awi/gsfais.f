C
C	$Id: gsfais.f,v 1.4 2000-08-22 15:08:19 haley Exp $
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
      SUBROUTINE GSFAIS (INTS)
C
C  SET FILL AREA INTERIOR STYLE
C
      INTEGER ESFAIS
      PARAMETER (ESFAIS=36)
C
      include 'gkscom.h'
C
      INTEGER INTS
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESFAIS,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that INTS is in range.
C
      IF (INTS.LT.0 .OR. INTS.GT.3) THEN
        ERS = 1
        CALL GERHND(2000,ESFAIS,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current index in the GKS state list.
C
      CFAIS = INTS
C
C  Invoke the workstation interface.
C
      FCODE = 38
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = INTS
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESFAIS,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
