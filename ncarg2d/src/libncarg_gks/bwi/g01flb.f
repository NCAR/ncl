C
C	$Id: g01flb.f,v 1.6 2008-07-27 00:21:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE G01FLB(GKSERR)
C
C  Write the metafile record out to the disk.
C
C  OUTPUT
C    GKSERR -- An error status flag.
C
C  All data is type integer unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
C
      include 'g01prm.h'
      include 'g01io.h'
C
      DIMENSION   FLAGS(12)
      CHARACTER*1 DNAME
C
C  Initialize flags array.
C
      DATA FLAGS( 1),FLAGS( 2),FLAGS( 3),FLAGS( 4)/0,0,0,0/
      DATA FLAGS( 5),FLAGS( 6),FLAGS( 7),FLAGS( 8)/0,0,0,0/
      DATA FLAGS( 9),FLAGS(10),FLAGS(11),FLAGS(12)/0,0,0,0/
C
C  Define the buffer reset point and clear the new frame flag.
C
      DATA BFREST,CLEAR/32,0/
C
C  Set the bit length for the ID part of a record, the number of bits per
C  byte, the field size for the record bit start for the record byte
C  length and the field size for the record byte length.
C
      DATA IDLEN,BYTSIZ,SIZST,SIZSZ/32,8,0,16/
C
C  Set the data type start bit in record, the data type length,
C  the new frame bit start bit, the new frame length.
C
      DATA MDTST,MDTSZ,MNFST,MNFSZ/16,4,20,1/
C
C  Enter the byte count.
C
      BCOUNT = (MBFPOS - IDLEN) / BYTSIZ
C
C  If the byte count is odd, then the final CGM element has an odd byte
C  count, so we should add a byte for the pad byte.
C
      IF (MOD(BCOUNT,2) .EQ. 1) THEN
        BCOUNT = BCOUNT+1
      ENDIF
C
C  Return if nothing in buffer.
C
      IF (BCOUNT .LE. 0) RETURN
      CALL SBYTES(MOUTBF,BCOUNT,SIZST,SIZSZ,0,1)
C
C  Enter the data type id.
C
      CALL SBYTES(MOUTBF,MDTYPE,MDTST,MDTSZ,0,1)
C
C  Put away new frame, begin metafile, and end metafile bits,
C  and then clear them.
C
      FLAGS(1) = MNFFLG
      FLAGS(2) = MBMFLG
      FLAGS(3) = MEMFLG
      CALL SBYTES(MOUTBF,FLAGS,MNFST,MNFSZ,0,12)
      MNFFLG = CLEAR
      MBMFLG = CLEAR
      MEMFLG = CLEAR
C
C  Write out the record.
C
      CALL G01MIO (3, MFGLUN, DNAME, MOUTBF, MOBFSZ, GKSERR)
      MRECNM = MRECNM + 1
C
C  Flush the system-level output buffer.
C
      CALL G01MIO (7, MFGLUN, DNAME, MOUTBF, MOBFSZ, GKSERR)
C
C  Reset the record pointer.
C
      MBFPOS = BFREST
C
      RETURN
      END
