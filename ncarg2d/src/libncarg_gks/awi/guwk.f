C
C	$Id: guwk.f,v 1.3 2000-07-12 16:40:01 haley Exp $
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
      SUBROUTINE GUWK(WKID,REGFL)
C
C  UPDATE WORKSTATION
C
      INTEGER EUWK
      PARAMETER (EUWK=8)
C
      include 'gkscom.h'
C
      INTEGER WKID,REGFL
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,EUWK,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if workstation identifier is valid.
C
      CALL GZCKWK(20,EUWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,EUWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if REGFL is in bounds.
C
      IF (REGFL.LT.0 .OR. REGFL.GT.1) THEN
        ERS = 1
        CALL GERHND(2000,EUWK,ERF)
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
      FCODE = 3
      CONT  = 0
      CALL GZROI(0)
      IL1 = 2
      IL2 = 2
      ID(1) = WKID
      ID(2) = REGFL
      CALL GZTOWK
      CUFLAG = -1
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,EUWK,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
