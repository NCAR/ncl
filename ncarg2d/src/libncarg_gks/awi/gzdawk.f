C
C	$Id: gzdawk.f,v 1.3 2000-08-22 15:08:29 haley Exp $
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
      SUBROUTINE GZDAWK(WKID)
C
C  DEACTIVATE WORKSTATION
C
      INTEGER EDAWK
      PARAMETER (EDAWK=5)
C
      include 'gkscom.h'
C
      INTEGER WKID
C
C  Check that GKS is in the proper state.
C
      CALL GZCKST(3,EDAWK,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation identifier is valid.
C
      CALL GZCKWK(20,EDAWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation is currently active.
C
      CALL GZCKWK(30,EDAWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Invoke the workstation interface so that the workstation
C  can be marked inactive in the workstation state list.
C
C  Set the flag CUFLAG to indicate that the interface call should go
C  only to the specifically designated workstation.
C
      CUFLAG = WKID
      FCODE = -1
      CONT  = 0
      CALL GZROI(0)
      IL1 = 1
      IL2 = 1
      ID(1) = WKID
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EDAWK,ERF)
        ERS = 0
      ENDIF
      CUFLAG = -1
C
C  Delete the workstation ID from the set of active workstations.
C
      IF (NACWK .EQ. 1) THEN
        SACWK(1) = -1
        NACWK = 0
      ELSE
        DO 201 I=1,NACWK
          IF (SACWK(I) .EQ. WKID) THEN
            IF (I .EQ. NACWK) THEN
              SACWK(NACWK) = -1
              NACWK = NACWK-1
            ELSE
              NM1 = NACWK-1
              DO 202 J=I,NM1
                SACWK(J) = SACWK(J+1)
  202         CONTINUE
              SACWK(NACWK) = -1
              NACWK = NACWK-1
            ENDIF
          ENDIF
  201   CONTINUE
      ENDIF
C
C
C  Set the GKS operating state to workstation open if there are
C  no active workstations.
C
      IF (NACWK .EQ. 0) THEN
        OPS = GWSOP
      ENDIF
C
      RETURN
      END
