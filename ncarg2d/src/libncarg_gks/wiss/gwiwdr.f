C
C	$Id: gwiwdr.f,v 1.10 2008-07-27 00:21:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWIWDR(ICNTX,RCNTX)
C
C***********************************************************************
C***********************************************************************
C**                                                                   **
C**                                                                   **
C**            Workstation driver for workstation type 3              **
C**         Workstation Independent Segment Storage  (WISS)           **
C**                                                                   **
C**                                                                   **
C***********************************************************************
C***********************************************************************
C
      IMPLICIT INTEGER (A-Z)
C
C  The current GKS attribut context is passed in via the arrays
C  ICNTX and RCNTX (see GZSRAT in awi for details on the order).
C
      INTEGER ICNTX(*)
      REAL    RCNTX(*)
C
C  Include all COMMONs .
C
      include 'gksin.h'
      include 'gwiwdt.h'
      include 'gwiwsl.h'
      include 'gwiadc.h'
      include 'gwiarq.h'
      include 'gwiast.h'
      include 'gwiadf.h'
      include 'gwiio.h'
      include 'gwiins.h'
      include 'gwiopc.h'
      include 'gwienu.h'
C
      SAVE
C
C  Initialize for this entry.
C
      RERR = 0
C
C  Check FCODE if last invocation indicated continuation.
C
      IF (MCONTS.EQ.1 .AND. FCODE.NE.MCODES)  GOTO 905
C
C  Copy and save function code and continuation flag.
C
        MCODES = FCODE
        MCONTS = CONT
C
C  Branch for function processing.  Testing of code organized to
C  minimize number of tests, by testing for most frequently expected
C  functions first.
C
C
C  Output primitives.
C
C      Code:  11   12   13   14   15   16
      GOTO  (110, 120, 130, 140, 150, 160)  MCODES-10
C
C  Primitive attributes.
C
      IF (MCODES.GE.21 .AND. MCODES.LE.43)  THEN
        CALL GWIPAT
        RETURN
      END IF
C
C  Control and escape functions.
C
C     Code:  -3   -2   -1    0    1
      GOTO (210, 220, 230, 240, 250,
     +      910, 270, 910, 910, 300)  MCODES+4
C     Code:   2    3    4    5    6
C
C
C   Segmentation functions.
C
      GOTO (420, 400, 410) MCODES-78
C
C  Clipping control function(s).
C
      IF (MCODES.EQ.61)  THEN
        CALL GWICLP(0)
        RETURN
      END IF
C
C  Fall through means undefined opcode.
C
        GO TO 910
C
C **********************************************************************
C *                                                                    *
C *          ========== OUTPUT PRIMITIVE PROCESSING ==========         *
C *                                                                    *
C **********************************************************************
C
C  POLYLINE.
C
  110 CONTINUE
      CALL GWIPL
      RETURN
C
C  POLYMARKER.
C
  120 CONTINUE
      CALL GWIPM
      RETURN
C
C  TEXT
C
  130 CONTINUE
      CALL GWITX
      RETURN
C
C  FILL AREA
C
  140 CONTINUE
      CALL GWIFA
      RETURN
C
C  CELL ARRAY
C
  150 CONTINUE
      CALL GWICA
      RETURN
C
C  GDP (Not supported, just generates error).
C
  160 CONTINUE
      RERR = 104
      RETURN
C
C **********************************************************************
C *                                                                    *
C *         ---------- CONTROL FUNCTION PROCESSING ----------          *
C *                                                                    *
C **********************************************************************
C
C  OPEN WORKSTATION
C
  210 CONTINUE
C
C  Mark workstation open in WSL, set connection ID
C
      MOPEN  = GYES
      WSSLUN = ID(2)
      RETURN
C
C  CREATE SEGMENT
C  
  400 CONTINUE
C
C  Initialize record number.
C
      WRECNM = 1
C
C  Open output file.
C
      ILEN = 0
      DO 10 I=1,80
        IF (STR(I:I).EQ.' ' .OR. STR(I:I).EQ.CHAR(0)) THEN
          ILEN = I-1
          GO TO 20
        ENDIF
   10 CONTINUE
   20 CONTINUE
      STR(ILEN+1:ILEN+1) = CHAR(0)
      CALL G01MIO (10, WSSLUN, STR(1:ILEN), WOUTBF, 1, RERR)
      IF (RERR .NE. 0)  RETURN
C
C  Initialize current-buffer-position pointer.
C
      WBFPOS = 32
C
C  Set the number of bits per record.
C
      WXBITS = 11520
C
C  Compute number of integer words needed for output buffer (WOUTBF).
C
      WOBFSZ = 1 + (WXBITS-1)/I1MACH(5)
C
C  Set data type for output records.
C
      WDTYPE = 3
C
C  Initialize workstation state list parameters.
C
      CALL  GWIIWS
C
C  Initialize attribute context.
C
      CALL  GWIIAC
C
C  Put the current attribute context into the segment.
C
      CALL GWIDMP(ICNTX,RCNTX)
C
      RETURN
C
C  Close segment.
C
  410 CONTINUE
      CALL GWIFLB (IER)
      CALL G01MIO (2, WSSLUN, STR, IDUM, 1, RERR)
      IL1 = 1
      IL2 = 1
      ID(1) = WRECNM-1
      RETURN
C
C  Delete segment.
C
  420 CONTINUE
      CALL GTNLEN(STR,ILEN,IER)
      STR(ILEN+1:ILEN+1) = CHAR(0)
      IF (STR(1:4) .EQ. 'GSEG') CALL G01MIO (9, 0, STR, IDUM, 1, RERR)
      RETURN
C
C  Activate workstation.
C
  220 CONTINUE
      MSTATE = GACTIV
      RETURN
C
C  Deactivate workstation.
C
  230 CONTINUE
      MSTATE = GINACT
      RETURN
C
C  Close workstation.
C
  240 CONTINUE
C
C  Mark workstation closed, flush buffer, close the output unit.
C
      MOPEN = GNO
      CALL GWIFLB (IER)
      CALL G01MIO (2, WSSLUN, STR, IDUM, 1, RERR)
      RETURN
C
C  Clear workstation.
C
  250 CONTINUE
      CALL GWICLW
      RETURN
C
C  Update workstation.
C
  270 CONTINUE
C
C  Flush output buffer.
C
      CALL GWIFLB (RERR)
      RETURN
C
C  ESCAPE.
C
  300 CONTINUE
      CALL GWIESC
      RETURN
C
C  New code on 1st WSI call after a call indicating continuation.
C
  905 CONTINUE
      RERR = 325
      RETURN
C
C  Undefined/unsupported opcode.
C
  910 CONTINUE
      RERR = 320
      RETURN
C
      END
