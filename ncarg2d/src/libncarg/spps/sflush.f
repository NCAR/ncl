C
C $Id: sflush.f,v 1.8 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SFLUSH
C
C SFLUSH flushes the GKS pen-move buffer as well as executing
C an UPDATE WORKSTATION on all open workstations.  In particular,
C for a CGM workstation, this effects a flush of the system level
C I/O buffers.
C
      INTEGER WKID
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFLUSH - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Flush the GKS pen move buffer.
C
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('SFLUSH',2).NE.0) RETURN
C
C Execute UPDATE WORKSTATION on all open workstations.
C
C If no workstations are open, return.
C
      CALL GQOPWK (1,IE,NO,ID)
      IF (IE.NE.0) THEN
        CALL SETER ('SFLUSH - ERROR EXIT FROM GQOPWK',3,1)
        RETURN
      END IF
      IF (NO .EQ. 0) RETURN
C
C Update all workstations.
C
      DO 200 I=1,NO
        CALL GQOPWK (I,IE,NO,WKID)
        IF (IE.NE.0) THEN
          CALL SETER ('SFLUSH - ERROR EXIT FROM GQOPWK',4,1)
          RETURN
        END IF
        CALL GUWK(WKID,0)
  200 CONTINUE
C
C Done.
C
      RETURN
C
      END
