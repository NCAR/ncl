C
C	$Id: gzdawk.f,v 1.4 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
