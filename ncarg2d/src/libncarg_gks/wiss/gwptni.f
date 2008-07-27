C
C	$Id: gwptni.f,v 1.4 2008-07-27 00:21:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWPTNI(OPCL, OPID, NBYTES, GKSERR)
C
C  This routine sets up the next instruction to be placed in the 
C  segment.      
C
C  After this call the GPUTPR or GPUTPS routines are called to 
C  load the operand list.
C
C
C  INPUT
C    OPCL   -- The opcode class of the instruction
C    OPID   -- the opcode id of the instruction
C    NBYTES -- the number of bytes in the operand list
C
C  OUTPUT
C    GKSERR -- The error status defined by common
C
C  All data is type intege unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
C
C  COMMON FOR COMMUNICATION OF INSTRUCTION AND LENGTH
C
      include 'gwiins.h'
C
      SAVE
C
C  Set the CGM data element partition size.
C  The partition size should be a multiple of 256 in order to
C  make the arithmetic in other parts of the code come out right.
C
      DATA PARSIZ/32256/
C
C  Load the current opcode class and id into common.
C
      MCOPCL = OPCL
      MCOPID = OPID
C
C  Set the current partition byte count and the remainder byte count.
C
      IF (NBYTES .GT. PARSIZ) THEN
        MCCBYT = PARSIZ
        MCNBYT = NBYTES - PARSIZ
      ELSE
        MCCBYT = NBYTES
        MCNBYT = 0
      END IF
C
C  Ready the current partition for the operand list.
C
      CALL GWELOD(GKSERR)
C
      RETURN
      END
