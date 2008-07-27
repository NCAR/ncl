C
C	$Id: gninst.f,v 1.4 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GNINST(IOS,STATUS)
C
C  Get the next instruction in the segment.
C
      include 'trpars.h'
      include 'trinst.h'
      include 'trbufr.h'
C
      INTEGER IOS,STATUS
      INTEGER MODPOS, CONVAL
C
      STATUS = 0
C       
C  Check if on a new instruction boundary.
C
      MODPOS = MOD(METBIT,MINSBD)
      IF (0 .NE. MODPOS) METBIT = METBIT + MODPOS
C
C  Check if there is room left for opcode extract in the buffer.
C
      IF (MRECLN .LT. (METBIT+MOPCLL+MOPIDL)) THEN
C
C  Read in a new record.
C
        CALL GSEGRD (IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
      END IF
C
C  Extract the opcode class.
C
      CALL GBYTES(MBUFER,OPCL,METBIT,MOPCLL,0,1)
      METBIT = METBIT + MOPCLL
C
C  Test for valid element class.
C
      IF (OPCL.LT.0 .OR. OPCL.GT.7) THEN
        STATUS = 7
        RETURN
      END IF
C
C  Read in element code.
C
      CALL GBYTES(MBUFER,OPID,METBIT,MOPIDL,0,1)
      METBIT = METBIT + MOPIDL
C
C  Test for valid element code.
C
      IF (OPID.LT.0 .OR. OPID.GT.127) THEN
        STATUS = 7
        RETURN
      END IF
C
C  Valid opcode, get the short count field.
C
      CALL GBYTES(MBUFER,LEN,METBIT,MSCLEN,0,1)
      METBIT = METBIT + MSCLEN
C
C  Test for the long count requirement.
C
      IF (LEN .LT. MLGFLG) THEN
C       
C  Short count valid, set pointers.
C
        CNTINU   = .FALSE.
        MOPRST = METBIT
        METBIT = METBIT + (LEN*BYTSIZ)
C
C  Return the short instruction format.
C
        RETURN
      END IF
C
C  Parse the long instruction format.
C
C  First test if there is enough room in the buffer.
C
      IF (MRECLN .LT. (METBIT+MCOLEN+MLOLEN)) THEN
C
C  Read in a new record.
C
        CALL GSEGRD (IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
      END IF
C
C  Get the continue flag value.
C
      CALL GBYTES(MBUFER,CONVAL,METBIT,MCOLEN,0,1) 
      METBIT = METBIT + MCOLEN
      IF (CONVAL .EQ. CONFLG) THEN
        CNTINU = .TRUE.
      ELSE
        CNTINU = .FALSE.
      END IF
C
C  Get the long operand 8 bit byte count field.
C
      CALL GBYTES(MBUFER,LEN,METBIT,MLOLEN,0,1)
      METBIT = METBIT + MLOLEN
C
      MOPRST = METBIT
      METBIT = METBIT + (LEN*BYTSIZ)
C
C  Normal exit.
C
      RETURN
      END
