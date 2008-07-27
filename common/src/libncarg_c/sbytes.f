C
C	$Id: sbytes.f,v 1.4 2008-07-27 12:23:45 haley Exp $
C                                                                      
C			     Copyright (C)  2000
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE SBYTES (NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C
C     This subroutine packs bits from the input array ISAM to the
C     output array NPACK.  After skipping "IBIT" bits in NPACK,
C     the "NBITS" rightmost bits from "ITER" successive words
C     in "ISAM" are packed into "NPACK" with "NSKIP" bits between each
C     moved block.
C
C
C     NPACK
C       Address of the first word of the array to be packed.
C
C     ISAM
C       Array to be packed into NPACK.  The right-most NBITS
C       bits of each word will be packed.  ISAM should be
C       dimensioned for at least ITER.
C
C     IBIT
C       A bit-count offset to be used before the first bits are
C       packed into NPACK.  For example, if IBIT=3 and NBITS=5,
C       3 bits in NPACK will be skipped before the right-most
C       5 bits of ISAM(1) are packed into it.
C
C     NBITS
C       The number of bits in each word of ISAM to be unpacked.
C       NBITS must not exceed the word size on the given machine.
C
C     NSKIP
C       The number of bits to skip in NPACK between packing each
C       bit chunk from ISAM.
C
C     ITER
C       The number of bit chunks to be packed.
C
C
C Example:
C
C     CALL SBYTES(NPC,ISB,45,6,3,2)
C
C In this call, 45 bits would be skipped at the beginning of NPC;
C the right-most 6 bits of ISB(1) would be packed into NPC; 3 bits
C would be skipped in NPC; the right-most 6 bits of ISB(2) would
C be packed into NPC.
C
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION ISAM(*),NPACK(*),MASK(64)
      SAVE NCALL,BPERI,MASK
      DATA NCALL/0/
C
C     Do initialization which needs to be done only on the
C     first call to SBYTES.
C
      IF (NCALL .EQ. 0) THEN
C
C       Get number of bits in an integer
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
      WRITE(6,*)  'SBYTES--NBITS OUT OF RANGE'
      STOP
   20 CONTINUE
C
C     Define bit mask to be used for the words in ISAM
C
      MSK1 = MASK(NBITS)
C
C     Define bit mask to be used for bits in NPACK
C
      IF (BPERI .EQ. NBITS) THEN
        MSK2 = 0
      ELSE
        MSK2 = MASK(BPERI-NBITS)
      ENDIF
C
C     Calculate the total number of bits in the output array
C     used by each move from words in the input array
C
      BITSEP = NBITS+NSKIP
C
C     Loop through the input array
C
      DO 30 I=1,ITER
C
C     Get bits to be moved from ISAM
C
      SBITS = IAND(MSK1,ISAM(I))
C
C     Calculate the bit position in the output array where
C     the bits from the input word are to go
C
      BPO = IBIT+(I-1)*BITSEP
C
C     Calculate the word position in the output array where
C     the bits from the input word are to go
C
      WPO = BPO/BPERI+1
C
C     Calculate the number of bits in the current output word
C     which still remain to be filled
C
      RBITS = BPERI*WPO-BPO
C
C     Calculate the number of bits in the current output word
C     which have already been filled
C
      UBITS = BPERI-RBITS
C
C     Treat the case where there are enough bits in the current
C     output word to receive the bits from the input word
C
      IF (RBITS .LT. NBITS) GO TO 40
C
C     Calculate the number of bits to left shift SBITS
C
      LSHIFT = BPERI-UBITS-NBITS
C
C     Fill the output word
C
      NPACK(WPO) = IAND(NPACK(WPO),ISHIFT(MSK2,LSHIFT+NBITS))
      NPACK(WPO) = IOR(NPACK(WPO),ISHIFT(SBITS,LSHIFT))
      GO TO 50
C
C     Treat the case where the moved bits cross a word boundary
C     in the output buffer
C
   40 CONTINUE
      RSHIFT = RBITS-NBITS
      BTMP = ISHIFT(SBITS,RSHIFT)
      NPACK(WPO) = IAND(NPACK(WPO),ISHIFT(MASK(UBITS),RBITS))
      NPACK(WPO) = IOR(NPACK(WPO),BTMP)
      BTMP = ISHIFT(IAND(MASK(-RSHIFT),SBITS),BPERI+RSHIFT)
      NPACK(WPO+1) = IAND(NPACK(WPO+1),MASK(BPERI+RSHIFT))
      NPACK(WPO+1) = IOR(NPACK(WPO+1),BTMP)
   50 CONTINUE
   30 CONTINUE
      RETURN
      END
