C
C	$Id: gbytes.f,v 1.4 2008-07-27 12:23:45 haley Exp $
C                                                                      
C			     Copyright (C)  2000
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE GBYTES (NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C
C     This subroutine unpacks bit chunks from the input array NPACK
C     into the output array ISAM.  "ITER" bit chunks of length
C     "NBITS" with "NSKIP" bits being skipped between each bit
C     chunk in the input array are unpacked into ISAM.  Each
C     bit chunk in the input array it unpacked into a separate
C     word in ISAM, right-justified with zero-fill.  Initially
C     "IBIT" bits will be skipped at the beginning of the
C     input array before unpacking.
C
C
C     NPACK
C       Address of the first word of the array to be unpacked.
C
C     ISAM
C       Array to receive the unpacked bit chunks.  They will be
C       right-justified with zero-fill in this array.  ISAM
C       should be dimensioned for ITER.
C
C     IBIT
C       A bit-count offset to be used before the first bit chunk is
C       unpacked.  For example, if IBIT=3 and NBITS=5, then
C       3 bits in NPACK will be skipped and the next 5 bits
C       will be unpacked into ISAM(1).
C
C     NBITS
C       The number of bits in each bit chunk to be unpacked.
C
C     NSKIP
C       The number of bits to skip between each bit chunk to be
C       unpacked (after the first bit chunk has been unpacked.)
C
C     ITER
C       The number of bit chunks to be unpacked.
C
C
C Example:
C
C      CALL GBYTES(NPB,ISB,3,6,9,2)
C
C In this call, 3 bits would be skipped at the beginning of NPB;
C the next 6 bits would be unpacked into ISB(1) right-justified with
C zero-fill; 9 bits would be skipped in NPB; the next six bits of
C NPB would be unpacked into ISB(2) right-justified with zero-fill.
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION NPACK(*),ISAM(*),MASK(64)
      SAVE NCALL,BPERI,MASK
      DATA NCALL/0/
C
C     Do initialization which needs to be done only on the
C     first call.
C
      IF (NCALL .EQ. 0) THEN
C
C       Get number of bits in an integer (word size)
C
        BPERI = I1MACH(5)
C
C       Set up masks
C
        MASK(1) = 1
        DO 10 I=2,BPERI
        MASK(I) = IOR(ISHIFT(MASK(I-1),1),1)
   10   CONTINUE
        NCALL = 1
      ENDIF
C
C     Check if NBITS is valid
C
      IF (NBITS.LE.BPERI .AND. NBITS.GT.0) GO TO 20
      WRITE(6,*) ' GBYTES--NBITS OUT OF RANGE'
      STOP
   20 CONTINUE
C
C     Define bit mask to be used for the words in ISAM
C
      MSK1 = MASK(NBITS)
C
C     Calculate the total number of bits used in the input array
C     by a bit-chunk move.
C
      BITSEP = NBITS+NSKIP
C
C     Fill the output array
C
      DO 50 I=1,ITER
C
C     Calculate the bit position in the input array where the bits
C     are to be unpacked.
C
      BPO = IBIT+(I-1)*BITSEP
C
C     Calculate the word position in the input array where the bits
C     are to be unpacked.
C
      WPO = BPO/BPERI+1
C
C     Calculate the number of bits in the current input word which
C     are to be unpacked.
C
      RBITS = BPERI*WPO-BPO
C
C     Compute shift.
C
      SHFT = NBITS-RBITS
C
C     Treat the case where there are enough bits in the current
C     word of the input word to be unpacked.
C
      IF (SHFT.GT.0) GO TO 30
      ISAM(I) = IAND(MSK1,ISHIFT(NPACK(WPO),SHFT))
      GO TO 40
C
C     Treat the case where the bits to be unpacked cross a word
C     boundary.
C
   30 CONTINUE
      BTMPL = ISHIFT(IAND(NPACK(WPO),MASK(RBITS)),SHFT)
      BTMPR = IAND(ISHIFT(NPACK(WPO+1),SHFT-BPERI),MASK(SHFT))
      ISAM(I) = IOR(BTMPL,BTMPR)
   40 CONTINUE
   50 CONTINUE
C
      RETURN
      END
