C
C	$Id: gspa.f,v 1.4 2000-08-22 15:08:20 haley Exp $
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
      SUBROUTINE GSPA (SZX,SZY)
C
C  SET PATTERN SIZE
C
C  Currently this subroutine does nothing since ctrans does not support
C  the CGM PATTERN SIZE element.
C
      INTEGER ESPA
      PARAMETER (ESPA=39)
C
      include 'gkscom.h'
C
      REAL SZX,SZY
C
C  Remove this RETURN and uncomment appropriate lines to 
C  activate the subroutine.
C	
      RETURN
C
C  Check if GKS is in the proper state.
C
C     CALL GZCKST(8,ESPA,IER)
C     IF (IER .NE. 0) RETURN
C
C  Check that the size specifications are valid.
C
C     IF (SZX.LE.0. .OR. SZY.LE.0.) THEN
C       ERS = 1
C       CALL GERHND(87,ESPA,ERF)
C       ERS = 0
C       RETURN
C     ENDIF
C
C  Set the current pattern size variables
C  in the gks state list (these remain in world coordinates).
C
C     CPA(1) = SZX
C     CPA(2) = SZY
C
C  Transform pattern sizes to NDC space.
C
C     CALL GZW2NX(1,SZX,SZXN)
C     CALL GZW2NY(1,SZY,SZYN)
C
C  Invoke the workstation interface.
C
C     FCODE = 41
C     CONT  = 0
C     CALL GZROI(0)
C     RL1   = 1
C     RL2   = 1
C     RX(1) = SZXN
C     RY(1) = SZYN
C     CALL GZTOWK
C     IF (RERR .NE. 0) THEN
C       ERS = 1
C       CALL GERHND(RERR,ESPA,ERF)
C       ERS = 0
C     ENDIF
C
C     RETURN
      END
