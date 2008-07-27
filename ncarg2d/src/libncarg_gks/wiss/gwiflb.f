C
C	$Id: gwiflb.f,v 1.4 2008-07-27 00:21:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWIFLB(GKSERR)
C
C  Write the record out to the disk.
C
C  OUTPUT
C    GKSERR -- The error status flag.
C
C  All data is type INTEGER unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
C
      include 'gwiio.h'
C
      DIMENSION  FLAGS(12)
      CHARACTER*1 DNAME
C
      SAVE
C
C  Initialize flags array.
C
      DATA FLAGS( 1),FLAGS( 2),FLAGS( 3),FLAGS( 4)/0,0,0,0/
      DATA FLAGS( 5),FLAGS( 6),FLAGS( 7),FLAGS( 8)/0,0,0,0/
      DATA FLAGS( 9),FLAGS(10),FLAGS(11),FLAGS(12)/0,0,0,0/
C
C  Define the buffer reset point.
C
      DATA BFREST/32/
C
C  Set the bit length for id part of a record, the number of bits per
C  byte, the field size for the record bit start for the record byte
C  length and the field size for the record byte length.
C
      DATA IDLEN,BYTSIZ,SIZST,SIZSZ/32,8,0,16/
C
C  Set the data type start bit in record, the data type length ,
C  the new frame bit start bit, the new frame length.
C
      DATA MDTST,MDTSZ,MNFST,MNFSZ/16,4,20,1/
C
C  Enter the byte count.
C
      BCOUNT = (WBFPOS - IDLEN) / BYTSIZ
C
C  If the byte count is odd, then the final CGM element has an odd byte
C  count, so we should add a byte for the pad byte.
C
      IF (MOD(BCOUNT,2) .EQ. 1) THEN
        BCOUNT = BCOUNT+1
      ENDIF
C
C  Return if nothing is in the buffer.
C
      IF (BCOUNT .LE. 0) RETURN
      CALL SBYTES(WOUTBF,BCOUNT,SIZST,SIZSZ,0,1)
C
C  Enter the data type ID.
C
      CALL SBYTES(WOUTBF,WDTYPE,MDTST,MDTSZ,0,1)
C
C  All metafile flags are zero.
C
      CALL SBYTES(WOUTBF,FLAGS,MNFST,MNFSZ,0,12)
C
C  Write out the record.
C
C     MRCNMO = MRECNM
C     MRECNM = WRECNM
      CALL G01MIO (3, WSSLUN, DNAME, WOUTBF, WOBFSZ, GKSERR)
      WRECNM = WRECNM + 1
C     MRECNM = MRCNMO
C
C  Reset the record pointer.
C
      WBFPOS = BFREST
C
      RETURN
C
      END
