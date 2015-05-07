C
C	$Id: g01wdr.f,v 1.13 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
        SUBROUTINE G01WDR(WKID,METANM)
C
C***********************************************************************
C***********************************************************************
C**                                                                   **
C**                                                                   **
C**            Workstation driver for workstation type 1              **
C**                       NCAR CGM generator                          **
C**                                                                   **
C**                                                                   **
C***********************************************************************
C***********************************************************************
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*(*) METANM
      CHARACTER*60 BEGSTR
C
C  Include all COMMONs .
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01wdt.h'
      include 'g01wsl.h'
      include 'g01adc.h'
      include 'g01rqa.h'
      include 'g01ast.h'
      include 'g01dfa.h'
      include 'g01io.h'
      include 'g01ins.h'
      include 'g01opc.h'
      include 'gksenu.h'
C
      INTEGER   G01PBL
      CHARACTER FNAME*1024,DNAME*1
C
      DATA BEGSTR
     - /'                                                            '/
C
C  Initialize for this entry.
C
C  Clear the workstation interface error parameter.
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
        CALL G01PAT
        RETURN
      END IF
C
C  Control and escape functions.
C
C     Code:  -6,  -5,  -4,  -3   -2   -1    0    1
      GOTO (180, 190, 200, 210, 220, 230, 240, 250,
     +      910, 270, 910, 910, 300)  MCODES+7
C     Code:   2    3    4    5    6
C
C
C  Clipping control function(s).
C
      IF (MCODES.EQ.61)  THEN
        CALL G01CLP(0)
        RETURN
      END IF
C
C  Color table.
C
      IF (MCODES.EQ.56)  THEN
        CALL G01CTB
        RETURN
      END IF
C
C  New picture initialization.
C
      IF (MCODES .EQ. 91)  THEN
        CALL G01SNP(RERR)
        RETURN
      END IF
C
C  Set picture name.
C
      IF (MCODES.EQ.92) THEN
        MPNAME(1:60) = STR(1:60)
        RETURN
      ENDIF
C
C  Message for ctrans pause.
C
      IF (MCODES.EQ.93) THEN
        CALL G01MSG(1)
        RETURN
      ENDIF
C
C  Workstation transformation.
C
      IF (MCODES.EQ.71 .OR. MCODES.EQ.72)  THEN
        CALL G01WKT
        RETURN
      END IF
C
C  Segmentation functions.
C
      IF (MCODES.GE.80 .AND. MCODES.LE.84) THEN
        CALL G01SEG
        RETURN
      ENDIF
C
C  WDT inquiry (-100 thru -199)
C
      IF (MCODES.LE.-100 .AND. MCODES.GE.-199)  THEN
        CALL G01DIQ
        RETURN
      END IF
C
C  WSL inquiry (-200 thru -299)
C
      IF (MCODES.LE.-200 .AND. MCODES.GE.-299)  THEN
        CALL G01SIQ
        RETURN
      END IF
C
C  Fall through means undefined opcode.
C
        GOTO 910
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
      CALL G01PL
      RETURN
C
C  POLYMARKER.
C
  120 CONTINUE
      CALL G01PM
      RETURN
C
C  TEXT
C
  130 CONTINUE
      CALL G01TX
      RETURN
C
C  FILL AREA
C
  140 CONTINUE
      CALL G01FA
      RETURN
C
C  CELL ARRAY
C
  150 CONTINUE
      CALL G01CA
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
C  Dump all attribute settings.
C
  180 CONTINUE
      CALL G01DMP(0)
      RETURN
C
C  Terminate workstation without putting out an END PICTURE or an
C  END METAFILE.
C
  190 CONTINUE
C
C  Mark workstation closed, flush buffer, close the output unit.
C
      MOPEN = GNO
C
C  Set parameters to tell G01CLW to conditionally finish picture and
C  not to put out an END PICTURE.
C
      ID(2) = GCONDI
      ID(3) = 0
      CALL G01CLW
C
C  Call central I/O routine to indicate file close.
C
      CALL G01MIO (2, MFGLUN, DNAME, MOUTBF, 1, RERR)
      RETURN
C
C  RE-OPEN WORKSTATION
C
  200 CONTINUE
C
      IF (CONT .EQ. 0) THEN
C
C  Call routine to flag memory resident files for UNIX ICTRANS use.
C
        CALL GOPNIC(IDUM)
C
C  Mark workstation open in WSL, set workstation ID, connection ID,
C  and workstation type.
C
        MOPEN  = GYES
        MWKID  = WKID
        MCONID = ID(2)
        MWTYPE = ID(3)
C
C  Set the file name.
C
        FNAME = METANM
        CALL GTNLEN(FNAME,ILEN,IER)
        IF (IER .EQ. 0) FNAME(ILEN+1:ILEN+1) = CHAR(0)
C
C  Initialize current-buffer-position pointer.
C
        MBFPOS = 32
C
C  Set number of bits per metacode record.
C
        MXBITS = 11520
C
C  Compute number of integer words needed for output buffer (MOUTBF).
C
        MOBFSZ = 1 + (MXBITS-1)/I1MACH(5)
C
C  Set data type for NCAR CGM.
C
        MDTYPE = 3
C
C  Copy LUN for output.
C
        MFGLUN = MCONID
C
C  Initialize begin-metafile and end-metafile flags.
C
        MBMFLG = GNO
        MEMFLG = GNO
C
C  Open metacode output unit.
C
        CALL G01MIO(11, MFGLUN, FNAME(1:ILEN), MOUTBF, MOBFSZ, RERR)
        IF (RERR .NE. 0)  RETURN
C
C  Determine the number of records in the file.
C
        MRECNM = 0
  100   CONTINUE
        CALL G01MIO(4, MFGLUN, DNAME, MOUTBF, MOBFSZ, RERR)
        IF (RERR .NE. 0) THEN
          IF (RERR .EQ. -1) THEN
            GO TO 105
          ELSE
            RETURN
          ENDIF
        ENDIF
        MRECNM = MRECNM+1
        GO TO 100
  105   CONTINUE
C
C  Back over the final record and read it.
C
        CALL G01MIO(7, MFGLUN, DNAME, MOUTBF, MOBFSZ, RERR)
        CALL G01MIO(6, MFGLUN, DNAME, IDM, 1, RERR)
        CALL G01MIO(7, MFGLUN, DNAME, MOUTBF, MOBFSZ, RERR)
        CALL G01MIO(4, MFGLUN, DNAME, MOUTBF, MOBFSZ, RERR)
        CALL G01MIO(7, MFGLUN, DNAME, MOUTBF, MOBFSZ, RERR)
C
C  If the record is an END METAFILE, back over it.
C
        CALL GBYTES(MOUTBF, IMFTMP, 0, 32, 0, 1)
        IF (IMFTMP .EQ. 143872) THEN
          CALL G01MIO(6, MFGLUN, DNAME, IDM, 1, RERR)
          CALL G01MIO(7, MFGLUN, DNAME, MOUTBF, MOBFSZ, RERR)
          IF (RERR .NE. 0)  RETURN
          MRECNM = MRECNM-1
        ENDIF
C
C  Back over the current last record and read it.
C
        CALL G01MIO(6, MFGLUN, DNAME, IDM, 1, RERR)
        CALL G01MIO(7, MFGLUN, DNAME, MOUTBF, MOBFSZ, RERR)
        CALL G01MIO(4, MFGLUN, DNAME, MOUTBF, MOBFSZ, RERR)
        CALL G01MIO(7, MFGLUN, DNAME, MOUTBF, MOBFSZ, RERR)
C
        CALL GBYTES(MOUTBF, IMFBSZ, 0, 16, 0, 1)
        CALL GBYTES(MOUTBF, IMFENP, (IMFBSZ+2)*8, 16, 0, 1)
        IF (IMFENP.NE.160 .AND. MRECNM.GT.1) THEN
C
C  No END PICTURE in current record; return an error flag of -10 to 
C  indicate a break in the middle of a picture; if the metafile
C  has only a single record (MRECNM = 1), then it doesn't have an
C  END PICTURE, but the reopen is still technically at a picture break,
C  so we do not flag a mid-picture file in that case.
C
          RERR = -10
          MDEMPT = GNEMPT
          RETURN
        ENDIF
C     
C  Initialize workstation state list parameters.
C
        CALL  G01IWS
C
C  Initialize attribute context.
C
        CALL  G01IAC
C
C  Initialize new-frame flag.
C
        MNFFLG = GNO
C
        RETURN
      ENDIF
C
C  OPEN WORKSTATION
C
  210 CONTINUE
C
C  Call routine to flag memory resident files for UNIX ICTRANS use.
C
      CALL GOPNIC(IDUM)
C
C  Mark workstation open in WSL, set workstation ID, connection ID,
C  and workstation type.
C
      MOPEN  = GYES
      MWKID  = WKID
      MCONID = ID(2)
      MWTYPE = ID(3)
C
C  Set the file name.
C
      IF (METANM(1:7) .EQ. 'DEFAULT') THEN
        FNAME = 'GMETA'
        ILEN = 5
      ELSE
        FNAME = METANM
        CALL GTNLEN(FNAME,ILEN,IER)
        FNAME(ILEN+1:ILEN+1) = CHAR(0)
      ENDIF
C
C  Initialize metacode record number.
C
      MRECNM = 1
C
C  Copy LUN for output.
C
      MFGLUN = MCONID
C
C  Open  metacode output unit.
C
      CALL G01MIO (1, MFGLUN, FNAME(1:ILEN), MOUTBF, 1, RERR)
      IF (RERR .NE. 0)  RETURN
C
C  Initialize current-buffer-position pointer.
C
      MBFPOS = 32
C
C  Set number of bits per metacode record.
C
      MXBITS = 11520
C
C  Compute number of integer words needed for output buffer (MOUTBF).
C
      MOBFSZ = 1 + (MXBITS-1)/I1MACH(5)
C
C  Set data type for NCAR CGM.
C
      MDTYPE = 3
C
C  Initialize workstation state list parameters.
C
      CALL  G01IWS
C
C  Initialize attribute context.
C
      CALL  G01IAC
C
C  Initialize new-frame, begin-metafile, end-metafile flags.
C
      MNFFLG = GNO
      MBMFLG = GYES
      MEMFLG = GNO
C
C  Generate BEGIN METAFILE element, 60 blank characters.
C
      CALL GPUTNI (CLBEGM, IDBEGM, G01PBL(60,0), RERR)
      CALL GPUTPS (BEGSTR, 60, 60, 0, RERR)
      IF (RERR .NE. 0)  RETURN
C
C  Put out METAFILE DESCRIPTOR and flush.
C
      CALL GPUTMD (RERR)
      CALL G01FLB (RERR)
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
C
C  Set parameter to tell G01CLW to conditionally finish picture and
C  put out an END PICTURE.
C
      ID(2) = GCONDI
      ID(3) = 1
      CALL G01CLW
C
C  Put out END METAFILE record.
C
      MNFFLG = GNO
      MEMFLG = GYES
      MBFPOS = 32
      CALL GPUTNI (CLENDM, IDENDM, 0, RERR)
      CALL G01FLB (RERR)
C
C  Call central I/O routine to indicate 'CLOSE WORKSTATION'.
C
      CALL G01MIO (2, MFGLUN, DNAME, MOUTBF, 1, RERR)
      RETURN
C
C  Clear workstation.
C
  250 CONTINUE
      ID(3) = 1
      CALL G01CLW
      RETURN
C
C  Update workstation.
C
  270 CONTINUE
C
C  Flush output buffer.
C
      CALL G01FLB (RERR)
      RETURN
C
C  ESCAPE.
C
  300 CONTINUE
      CALL G01ESC
      RETURN
C
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
