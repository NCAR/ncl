C
C	$Id: gschxp.f,v 1.4 2000-08-22 15:08:17 haley Exp $
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
      SUBROUTINE GSCHXP (CHXP)
C
C  SET CHARACTER EXPANSION FACTOR
C
      INTEGER ESCHXP
      PARAMETER (ESCHXP=28)
C
      include 'gkscom.h'
C
      REAL CHXP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESCHXP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the expansion factor is valid.
C
      IF (CHXP.LE.0.) THEN
        ERS = 1
        CALL GERHND(77,ESCHXP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current expansion factor in the GKS state list.
C
      CCHXP = CHXP
C
C  Invoke the workstation interface.
C
      FCODE = 31
      CONT  = 0
      CALL GZROI(0)
      RL1   = 1
      RL2   = 1
      RX(1) = CHXP
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESCHXP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
