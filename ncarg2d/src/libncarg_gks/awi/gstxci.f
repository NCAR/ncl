C
C	$Id: gstxci.f,v 1.3 2000-07-12 16:39:59 haley Exp $
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
      SUBROUTINE GSTXCI (COLI)
C
C  SET TEXT COLOUR INDEX
C
      INTEGER ESTXCI
      PARAMETER (ESTXCI=30)
C
      include 'gkscom.h'
C
      INTEGER COLI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESTXCI,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the index is valid.
C
      IF (COLI .LT. 0) THEN
        ERS = 1
        CALL GERHND(92,ESTXCI,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current color index in the GKS state list.
C
      CTXCI = COLI
C
C  Invoke the workstation interface.
C
      FCODE = 33
      CONT  = 0
      CALL GZROI(0)
      IC1   = 1
      IC2   = 1
      IC(1) = COLI
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESTXCI,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
