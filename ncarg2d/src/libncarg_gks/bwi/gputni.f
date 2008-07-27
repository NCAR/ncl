C
C	$Id: gputni.f,v 1.5 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GPUTNI(OPCL, OPID, NBYTES, GKSERR)
C
C  This routine sets up the next instruction to be placed in
C  the metafile.
C
C  After this call the GPUTPR or GPUTPS routines are called to load
C  the operand list.
C
C
C  INPUT
C    OPCL   -- The opcode CLASS of the element.
C    OPID   -- The opcode ID of the element.
C    NBYTES -- The number of bytes in the operand list.
C
C  OUTPUT
C    GKSERR -- An error status return.
C
C  All data is type intege unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
C
      include 'g01prm.h'
      include 'g01ins.h'
C
C  Set the CGM data element partition size.  The partition size 
C  should be a multiple of 256 in order to  make the arithmetic 
C  in other parts of the code come out right.
C
      DATA PARSIZ/32256/
C
C  Load the current opcode CLASS and ID into common.
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
      CALL GINLOD(GKSERR)
C
      RETURN
      END
