C
C	$Id: gwiflb.f,v 1.2 2000-07-12 16:54:42 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
