C
C	$Id: gsparf.f,v 1.3 2000-07-12 16:39:58 haley Exp $
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
      SUBROUTINE GSPARF (RFX,RFY)
C
C  SET PATTERN REFERENCE POINT
C
C  Currently this subroutine does nothing since ctrans does not support
C  the CGM PATTERN SIZE element.
C
      INTEGER ESPARF
      PARAMETER (ESPARF=40)
C
      include 'gkscom.h'
C
      REAL RFX,RFY
C
C  Remove this RETURN and uncomment appropriate lines 
C  to activate the subroutine.
C
      RETURN
C
C  Check if GKS is in the proper state.
C
C     CALL GZCKST(8,ESPARF,IER)
C     IF (IER .NE. 0) RETURN
C
C  Set the current pattern reference point
C  in the GKS state list (these remain in world coordinates).
C
C     CPARF(1) = RFX
C     CPARF(2) = RFY
C
C  Transform pattern sizes to NDC space.
C
C     CALL GZW2NX(1,RFX,RFXN)
C     CALL GZW2NY(1,RFY,RFYN)
C
C  INVOKE THE WORKSTATION INTERFACE
C
C     FCODE = 42
C     CONT  = 0
C     CALL GZROI(0)
C     RL1   = 2
C     RL2   = 2
C     RX(1) = RFXN
C     RX(2) = RFYN
C     CALL GZTOWK
C     IF (RERR.NE.0 .AND. RERR.NE.-109) THEN
C       ERS = 1
C       CALL GERHND(RERR,ESPARF,ERF)
C       ERS = 0
C     ENDIF
C
C     RETURN
      END
