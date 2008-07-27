C
C $Id: frame.f,v 1.9 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FRAME
      COMMON /GFLASH/MODEF,IOPWKS(100),IOACT(100),NUMOP,IWISSI
C
C  FRAME is designed to effect a break in the picture drawing
C  sequence depending upon whether the workstation type is 
C  MO, or whether it is an OUTPUT or OUTIN workstation.
C  
C  An UPDATE WORKSTATION and CLEAR WORKSTATION is done on all 
C  metafiles and all workstations of type OUTPUT.  For metafiles
C  this inserts an END PICTURE into the metafile.
C  
C  If there are any OUTIN workstations, all of them are updated
C  with an UPDATE WORKSTATION and a pause is done on the OUTIN
C  workstation of most recent creation.  After return from the 
C  pause, a CLEAR WORKSTATION is done on all OUTIN workstations.
C
      INTEGER WKID
      CHARACTER*80 DATREC,STR,ISTR
C
C Check for an uncleared prior error.
C
      IF (ICFELL('FRAME - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C  First, flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('FRAME',2).NE.0) RETURN
C
C  If no workstations are open, return.
C
      CALL GQOPWK (1,IE,NO,ID)
      IF (IE.NE.0) THEN
        CALL SETER ('FRAME - ERROR EXIT FROM GQOPWK',3,1)
        RETURN
      END IF
      IF (NO .EQ. 0) RETURN
C
C  Update all workstations.
C
      DO 200 I=1,NO
C
C  Get the workstation ID.
C
        CALL GQOPWK (I,IE,NO,WKID)
        IF (IE.NE.0) THEN
          CALL SETER ('FRAME - ERROR EXIT FROM GQOPWK',4,1)
          RETURN
        END IF
C
C  Get workstation type.
C
        CALL GQWKC (WKID,IE,ICON,ITYPE)
        IF (IE.NE.0) THEN
          CALL SETER ('FRAME - ERROR EXIT FROM GQWKC',5,1)
          RETURN
        END IF
C
C  Get workstation category (0=output; 2=out/in; 4=metafile).
C
        CALL GQWKCA (ITYPE,IE,ICAT)
        IF (IE.NE.0) THEN
          CALL SETER ('FRAME - ERROR EXIT FROM GQWKCA',6,1)
          RETURN
        END IF
C
        IF (ICAT .EQ. 4) THEN
C
C  Illegal to call FRAME while a FLASH buffer is open.
C
          IF (MODEF .EQ. 1) THEN
            CALL SETER 
     -    ('FRAME - ILLEGAL TO CALL FRAME WHILE A FLASH BUFFER IS OPEN',
     -      7,1)
          ENDIF
          CALL GCLRWK(WKID,1)
        ELSE IF (ICAT.EQ.0 .OR. ICAT.EQ.2) THEN
          CALL GUWK(WKID,0)
          IF (ICAT .EQ. 0) THEN
            CALL GCLRWK(WKID,1)
          ENDIF
        ENDIF
  200 CONTINUE
C
C  Pause on the OUTIN workstation of most recent creation.
C
      DO 100 I=NO,1,-1
        CALL GQOPWK (I,IE,NO,WKID)
        IF (IE.NE.0) THEN
          CALL SETER ('FRAME - ERROR EXIT FROM GQOPWK',8,1)
          RETURN
        END IF
        CALL GQWKC (WKID,IE,ICON,ITYPE)
        IF (IE.NE.0) THEN
          CALL SETER ('FRAME - ERROR EXIT FROM GQWKC',9,1)
          RETURN
        END IF
        CALL GQWKCA (ITYPE,IE,ICAT)
        IF (IE.NE.0) THEN
          CALL SETER ('FRAME - ERROR EXIT FROM GQWKCA',10,1)
          RETURN
        END IF
        IF (ICAT.EQ.2) THEN
          ISTR(1:1) = CHAR(0)
          CALL GINST(WKID,1,0,ISTR,1,0.,1279.,0.,1023.,1,1,1,DATREC)
          CALL GSSTM(WKID,1,0,0)
          CALL GRQST(WKID,1,ISTAT,LOSTR,STR)
          GO TO 110
        ENDIF
  100 CONTINUE
  110 CONTINUE
C
C  Clear all OUTIN workstations.
C
      DO 300 I=1,NO
        CALL GQOPWK (I,IE,NO,WKID)
        IF (IE.NE.0) THEN
          CALL SETER ('FRAME - ERROR EXIT FROM GQOPWK',11,1)
          RETURN
        END IF
        CALL GQWKC (WKID,IE,ICON,ITYPE)
        IF (IE.NE.0) THEN
          CALL SETER ('FRAME - ERROR EXIT FROM GQWKC',12,1)
          RETURN
        END IF
        CALL GQWKCA (ITYPE,IE,ICAT)
        IF (IE.NE.0) THEN
          CALL SETER ('FRAME - ERROR EXIT FROM GQWKCA',13,1)
          RETURN
        END IF
        IF (ICAT.EQ.2) THEN
          CALL GCLRWK(WKID,1)
        ENDIF
  300 CONTINUE
      RETURN
      END
