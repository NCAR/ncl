C
C	$Id: gzacwk.f,v 1.3 2000-08-22 15:08:26 haley Exp $
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
      SUBROUTINE GZACWK(WKID)
C
C  ACTIVATE WORKSTATION
C
      INTEGER EACWK
      PARAMETER (EACWK=4)
C
      include 'gkscom.h'
C
      INTEGER WKID
C
C  Check that GKS is in the proper state.
C
      CALL GZCKST(6,EACWK,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation identifier is valid.
C
      CALL GZCKWK(20,EACWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,EACWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation is currently active.
C
      CALL GZCKWK(29,EACWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if there is room for another active workstation.
C
      IF (NACWK .GE. MACWK) THEN
        ERS = 1
        CALL GERHND(-101,EACWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the GKS operating state to WSAC.
C
      OPS = GWSAC
C
C  Add the workstation identifier to the set of active workstations
C  in the GKS state list.
C
      NACWK = NACWK+1
      SACWK(NACWK) = WKID
C
C  Invoke the workstation interface so that the workstation
C  can be marked active in the workstation state list.
C
C  Set the flag CUFLAG to indicate that the interface call 
C  should go only to the corresponding open workstation.
C
      CUFLAG = WKID
      FCODE = -2
      CONT  = 0
      CALL GZROI(0)
      IL1 = 1
      IL2 = 1
      ID(1) = WKID
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EACWK,ERF)
        ERS = 0
        RETURN
      ENDIF
      CUFLAG = -1
C
      RETURN
      END
