C
C	$Id: frame.f,v 1.1.1.1 1992-04-17 22:32:29 ncargd Exp $
C
      SUBROUTINE FRAME
      COMMON /GFLASH/MODEF,IOPWKS(100),IOACT(100),NUMOP,IWISSI
C
C FRAME is intended to advance to a new frame.  The GKS version clears
C all open workstations.
C
C Illegal to call frame while a FLASH buffer is open.
C
      IF (MODEF .EQ. 1) THEN
        CALL SETER ('FRAME  - ILLEGAL TO CALL FRAME WHILE A FLASH BUFFER
     -IS OPEN',16,2)
      ENDIF
C
C First, flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
C
C Get the number of open workstations.  If there are none, we're done.
C
      CALL GQOPWK (0,IE,NO,ID)
      IF (NO.EQ.0) RETURN
C
C Otherwise, clear the open workstations of category OUTPUT, OUTIN,
C or MO.
C
      DO 101 I=1,NO
C
C Get workstation ID.
C
        CALL GQOPWK (I,IE,NO,ID)
C
C Get workstation type.
C
        CALL GQWKC (ID,IE,IA,IT)
C
C Get workstation category.
C
        CALL GQWKCA (IT,IE,IC)
C
C Clear workstation if it is of category OUTPUT, OUTIN, or MO.
C
        IF (IC.EQ.0 .OR. IC.EQ.2 .OR. IC.EQ.4) CALL GCLRWK (ID,1)
  101 CONTINUE
C
C Done.
C
      RETURN
C
      END
