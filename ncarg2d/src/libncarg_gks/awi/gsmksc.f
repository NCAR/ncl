C
C	$Id: gsmksc.f,v 1.4 2000-08-22 15:08:20 haley Exp $
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
      SUBROUTINE GSMKSC (MSZSF)
C
C  SET MARKER SIZE SCALE FACTOR
C
      INTEGER ESMKSC
      PARAMETER (ESMKSC=24)
C
      include 'gkscom.h'
C
      REAL MSZSF
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESMKSC,IER)
      IF (IER .NE. 0) RETURN
C
C  Set the current marker scale factor in the GKS state list.
C
      CMKS = MSZSF
C
C  Invoke the workstation interface.
C
      FCODE = 27
      CONT  = 0
      CALL GZROI(0)
      RL1   = 1
      RL2   = 1
      RX(1) = MSZSF
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESMKSC,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
