C
C $Id: sflush.f,v 1.3 1993-12-12 20:56:23 kennison Exp $
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
C Flush the GKS pen move buffer.
C
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('SFLUSH',1).NE.0) RETURN
C
C Execute UPDATE WORKSTATION on all open workstations.
C
C
C  If no workstations are open, return.
C
      CALL GQOPWK (1,IE,NO,ID)
      IF (IE.NE.0) THEN
        CALL SETER ('SFLUSH - ERROR EXIT FROM GQOPWK',2,1)
        RETURN
      END IF
      IF (NO .EQ. 0) RETURN
C
C  Update all workstations.
C
      DO 200 I=1,NO
        CALL GQOPWK (I,IE,NO,WKID)
        IF (IE.NE.0) THEN
          CALL SETER ('SFLUSH - ERROR EXIT FROM GQOPWK',3,1)
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
