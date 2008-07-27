C
C	$Id: g01tx.f,v 1.9 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE G01TX
C
C  Process TEXT elements.
C
C ************************************************************************
C **                                                                    **
C **     NOTE:  Continuation across character partitions is not         **
C **            implemented.  This limits character strings to          **
C **            lengths less than 32768.                                **
C **                                                                    **
C ************************************************************************
C
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01io.h'
      include 'g01ins.h'
      include 'g01wsl.h'
      include 'g01rqa.h'
      include 'g01ast.h'
      include 'g01adc.h'
      include 'g01opc.h'
      include 'gksenu.h'
C
      INTEGER  NUMO(2), KALL, IPRIM, NBYTES, GFINAL, G01PBL
      SAVE KALL
C
      DATA KALL/0/,  IPRIM/3/,  GFINAL/1/
C
      KALL = KALL+1
      IF  (KALL.EQ.1) THEN
C
C  First call of text element.
C
C  Set the WSL entry "DISPLAY SURFACE EMPTY" to "NOT EMPTY".
C
        MDEMPT = GNEMPT
C
C  Process pending attributes.
C
        IF (AGPEND(IPRIM)) THEN
C
C  Some changes are pending.
C
          IF (VALCHG(IVTXIX))  THEN
C
C  TEXT INDEX, send CGM text bundle index.
C
            NBYTES = 1+(MIXFW-1)/8
            CALL GPUTNI (CLTBIX, IDTBIX, NBYTES, RERR)
            CALL GPUTPR (MRTXIX, MIXFW, 1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSTXIX         = MRTXIX
            VALCHG(IVTXIX) = .FALSE.
          END IF
          IF (VALCHG(IVTXFO))  THEN
C
C  FONT component of GKS TEXT FONT/PRECISION, send CGM text precision.
C
            NBYTES = 1+(MEFW-1)/8
            CALL GPUTNI (CLTFON, IDTFON, NBYTES, RERR)
            CALL GPUTPR (MRTXFO, MEFW, 1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSTXFO         = MRTXFO
            VALCHG(IVTXFO) = .FALSE.
          END IF
          IF (VALCHG(IVTXPR)) THEN
C
C  PRECISION component of GKS TEXT FONT/PRECISION, send CGM text precsion.
C
            NBYTES         = 1+(MIXFW-1)/8
            CALL GPUTNI (CLTPRE, IDTPRE, NBYTES, RERR)
            CALL GPUTPR (MRTXPR, MIXFW, 1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSTXPR         = MRTXPR
            VALCHG(IVTXPR) = .FALSE.
          END IF
          IF (VALCHG(IVCHXP)) THEN
C
C  CHARACTER EXPANSION FACTOR, send CGM character expansion factor.
C
            NBYTES = 1+(2*MCFPP-1)/8
            CALL GPUTNI (CLCHEX, IDCHEX, NBYTES, RERR)
            CALL GFLCNV (ARCHXP, NUMO)
            CALL GPUTPR (NUMO, MCFPP, 2, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            ASCHXP         = ARCHXP
            VALCHG(IVCHXP) = .FALSE.
          END IF
          IF (VALCHG(IVCHSP)) THEN
C
C  CHARACTER SPACING, send CGM character spacing.
C
            NBYTES = 1+(2*MCFPP-1)/8
            CALL GPUTNI (CLCHSP, IDCHSP, NBYTES, RERR)
            CALL GFLCNV (ARCHSP, NUMO)
            CALL GPUTPR (NUMO, MCFPP, 2, RERR)
C
C  Set sent value to requested, clear change flag.
C
            ASCHSP         = ARCHSP
            VALCHG(IVCHSP) = .FALSE.
          END IF
          IF (VALCHG(IVTXCI)) THEN
C
C  TEXT COLOR INDEX, send CGM text color.
C
            NBYTES = 1+(MCIXFW-1)/8
            CALL GPUTNI (CLTCLR, IDTCLR, NBYTES, RERR)
            CALL GPUTPR (MRTXCI, MCIXFW, 1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSTXCI         = MRTXCI
            VALCHG(IVTXCI) = .FALSE.
          END IF
          IF (VALCHG(IVTXP)) THEN
C
C  TEXT PATH, send CGM text path.
C
            NBYTES = 1+(MEFW-1)/8
            CALL GPUTNI (CLTXPA, IDTXPA, NBYTES, RERR)
            CALL GPUTPR (MRTXP , MEFW,  1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSTXP         = MRTXP
            VALCHG(IVTXP) = .FALSE.
          END IF
          IF (VALCHG(IVTXAL)) THEN
C
C  TEXT ALIGNMENT, send CGM text alignment.
C
            NBYTES = 1+(2*MEFW + 4*MCFPP - 1)/8
            CALL GPUTNI (CLTXAL, IDTXAL, NBYTES, RERR)
C
C  Put in GKS/CGM discrete alignment values.
C
            CALL GPUTPR (MRTXAL, MEFW, 2, RERR)
C
C  Add (unused) CGM continuous parameters.
C
            NUMO(1) = 0
            NUMO(2) = 0
            CALL GPUTPR (NUMO, MCFPP, 2, RERR)
            CALL GPUTPR (NUMO, MCFPP, 2, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSTXAL(1)      = MRTXAL(1)
            MSTXAL(2)      = MRTXAL(2)
            VALCHG(IVTXAL) = .FALSE.
          END IF
          IF (VALCHG(IVCHH)) THEN
C
C  CHARACTER HEIGHT (derived from interface vector pair),
C  put out CGM character height.
C
            NBYTES = 1+(MVDCFW-1)/8
            CALL GPUTNI (CLCHHT, IDCHHT, NBYTES, RERR)
            CALL GPUTPR (MRCHH, MVDCFW,  1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSCHH         = MRCHH
            VALCHG(IVCHH) = .FALSE.
          END IF
          IF (VALCHG(IVCHOV)) THEN
C
C  CHARACTER UP VECTOR (as per interface vector pair),
C  send CGM character orientation (vector pair).
C
            NBYTES = 1+(4*MVDCFW-1)/8
            CALL GPUTNI (CLCHOR, IDCHOR, NBYTES, RERR)
            CALL GPUTPR (MRCHOV(1) , MVDCFW,  4, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSCHOV(1)      = MRCHOV(1)
            MSCHOV(2)      = MRCHOV(2)
            MSCHOV(3)      = MRCHOV(3)
            MSCHOV(4)      = MRCHOV(4)
            VALCHG(IVCHOV) = .FALSE.
          END IF
          IF (ANYASF)  THEN
C
C  Some ASF has changed.
C
            CALL G01SAS (IPRIM, RERR)
            IF (RERR.NE.0)  RETURN
          END IF
C
C  Clear aggregate change variable.
C
           AGPEND(IPRIM) = .FALSE.
        END IF
C
C  Treat first call, put out opcode, and points.
C
C  Put out opcode (CLASS and ID) and total LENGTH.
C
        NBYTES = G01PBL (STRL1, 1+(MEFW+2*MVDCFW-1)/8 )
        CALL GPUTNI (CLTEXT, IDTEXT, NBYTES, RERR)
C
C  Put out starting point.
C
C  Truncate point to limits of the NDC unit square, convert to VDC,
C  and store in MPXPY.
C
        MPXPY(1) = MXOFF + INT(REAL(MXSCAL)*
     +             (MAX(0.,MIN(1.0,RX(1)))))
        MPXPY(2) = MYOFF + INT(REAL(MYSCAL)*
     +             (MAX(0.,MIN(1.0,RY(1)))))
        CALL GPUTPR (MPXPY, MVDCFW, 2, RERR)
C
C  Put out final flag.
C
        CALL GPUTPR (GFINAL, MEFW, 1, RERR)
C
C  Put out first (and perhaps only) part of character string.
C
        CALL GPUTPS (STR, STRL1, STRL2, 0, RERR)
        IF (RERR .NE. 0) RETURN
C
C  End of processing for first call of element.
C
      ELSE IF (KALL .GT. 1) THEN
C
C  This is a continuation call for the element.
C
        CALL GPUTPS (STR, STRL1, STRL2, 1, RERR)
        IF (RERR .NE. 0) RETURN
      END IF
C
C  If there is to be no continuation, reset the parameter "KALL".
C
      IF (CONT .EQ. 0) THEN
         KALL = 0
      END IF
C
      RETURN
      END
