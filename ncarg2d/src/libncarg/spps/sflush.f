C
C	$Id: sflush.f,v 1.2 1993-01-14 19:54:41 fred Exp $
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
C
C Execute UPDATE WORKSTATION on all open workstations.
C
C
C  If no workstations are open, return.
C
      CALL GQOPWK (1,IER,NO,ID)
      IF (NO .EQ. 0) RETURN
C
C  Update all workstations.
C
      DO 200 I=1,NO
        CALL GQOPWK (I,IERR,NO,WKID)
        CALL GUWK(WKID,0)
  200 CONTINUE
C
C Done.
C
      RETURN
C
      END
