C
C	$Id: gswkwn.f,v 1.4 2000-08-22 15:08:23 haley Exp $
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
      SUBROUTINE GSWKWN (WKID,XMIN,XMAX,YMIN,YMAX)
C
C  SET WORKSTATION WINDOW
C
      INTEGER ESWKWN
      PARAMETER (ESWKWN=54)
C
      include 'gkscom.h'
C
      INTEGER WKID
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,ESWKWN,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if workstation identifier is valid.
C
      CALL GZCKWK(20,ESWKWN,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,ESWKWN,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the rectangle definition is valid.
C
      IF (XMAX.LE.XMIN .OR. YMAX.LE.YMIN) THEN
        ERS = 1
        CALL GERHND(51,ESWKWN,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that the window lies in NDC space.
C
      IF (XMIN.LT.0. .OR. XMAX.GT.1. .OR.
     +    YMIN.LT.0. .OR. YMAX.GT.1.) THEN
        ERS = 1
        CALL GERHND(53,ESWKWN,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Invoke the workstation interface.
C
C  Set the flag CUFLAG to indicate that the interface call should go
C  only to the specifically designated workstation.
C
      CUFLAG = WKID
      FCODE = 71
      CONT  = 0
      CALL GZROI(0)
      IL1  = 1
      IL2  = 1
      ID(1) = WKID
      RL1 = 2
      RL2 = 2
      RX(1) = XMIN
      RX(2) = XMAX
      RY(1) = YMIN
      RY(2) = YMAX
      CONT = 0
      CALL GZTOWK
      CUFLAG = -1
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESWKWN,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
