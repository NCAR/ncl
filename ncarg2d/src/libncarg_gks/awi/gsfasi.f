C
C	$Id: gsfasi.f,v 1.3 2000-07-12 16:39:57 haley Exp $
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
      SUBROUTINE GSFASI (STYLI)
C
C  SET FILL AREA STYLE INDEX
C
      INTEGER ESFASI
      PARAMETER (ESFASI=37)
C
      include 'gkscom.h'
C
      INTEGER STYLI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESFASI,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the index is valid.
C
      IF (STYLI .LE. 0) THEN
        ERS = 1
        CALL GERHND(84,ESFASI,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current index in the GKS state list.
C
      CFASI = STYLI
C
C  Invoke the workstation interface.
C
      FCODE = 39
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = STYLI
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESFASI,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
