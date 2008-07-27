C
C $Id: pcexcd.f,v 1.10 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCEXCD (IPNT,IPSS,NDGU)
C
C The subroutine PCEXCD extracts a specified character digitization
C from the PWRITX database.  Its arguments are as follows:
C
C   IPNT is an input variable.  It is given a positive integral value
C   and identifies a particular character digitization by specifying a
C   particular 15-bit pointer in the COMMON array INDA.
C
C   IPSS is an input variable. It specifies how much of the digitization
C   is required by the caller.  If IPSS is 1, the length of a character
C   string is being determined, and only the first two units of the
C   digitization are needed.  If IPSS is 2, the character string is
C   actually being drawn, and all units are needed.
C
C   NDGU is an output variable.  It is set to zero if IPNT specifies an
C   illegal digitization; otherwise, its value specifies how many units
C   have been returned in the COMMON array RDGU.
C
C Each word of the COMMON array INDA contains as many 15-bit pointers
C as will fit, packed together at the upper end of the word; any unused
C bits in the word are at the right end.  Each of the 15-bit pointers
C consists of a single bit which is zero if the digitization uses 6-bit
C units and 1 if it uses 12-bit units, followed by a 14-bit pointer
C into the COMMON array IDDA.
C
C The COMMON array IDDA contains character digitizations, each of which
C consists of a string of 6-bit or 12-bit quantities.  The digitization
C "n" starts at bit number 60*(n-1)+1 and continues until two zeroed
C units in a row are found.  The 6-bit or 12-bit units are stored as
C a bit stream and may cross one or more word boundaries.
C
C Each 6-bit or 12-bit unit of a digitization is transformed before
C being put into RDGU.  From each non-zero 6-bit unit, 32 is subtracted.
C From each non-zero 12-bit unit, 2048 is subtracted.  Each zeroed unit
C is replaced by the value -2048.
C
C COMMON block declarations.
C
      COMMON /PCSVEM/ ICOD,IDDA(8625),IDDL,RDGU(7000),IDPC(256),IERU,
     +                INDA(789),INDL,INIT,IVCO,IVDU,NBPW,NPPW
      SAVE   /PCSVEM/
C
C MXUN specifies the maximum number of units to be retrieved for a
C given value of IPSS.
C
      DIMENSION MXUN(2)
C
      DATA MXUN / 2,7000 /
C
C Zero the count of digitization units returned.
C
      NDGU=0
C
C If the given pointer is invalid, return.
C
      IF (IPNT.LE.0.OR.IPNT.GT.768) RETURN
C
C Retrieve the proper pointer from the array INDA.
C
      INDI=(IPNT-1)/NPPW+1
      IPIW=IPNT-(INDI-1)*NPPW
      IDGP=IAND(ISHIFT(INDA(INDI),IPIW*15-NBPW),32767)
C
C Separate the 6-bit/12-bit mode indicator from the pointer.
C
      IMDE=IAND(ISHIFT(IDGP,-14),1)
      IDGP=IAND(IDGP,16383)
C
C If there is no digitization for the specified pointer, return.
C
      IF (IDGP.EQ.0) RETURN
C
C Initialize the digitization-unit retrieval loop.  IDDI is the index
C of the word in IDDA containing the first bit of the next unit and
C IFBT is the index of that bit within the word, with zero indicating
C the leftmost bit.
C
      IFBT=(IDGP-1)*60
C
      IDDI=IFBT/NBPW+1
      IFBT=MOD(IFBT,NBPW)
C
C NBPU is the number of bits per digitization unit, MASK is a mask of
C NBPU right-justified one bits, and IOFF is the offset to be subtracted
C from the digitization unit.
C
      IF (IMDE.EQ.0) THEN
        NBPU=6
        MASK=63
        IOFF=32
      ELSE
        NBPU=12
        MASK=4095
        IOFF=2048
      END IF
C
C Extract the digitization.  NRGT is the number of bits in the "right
C end" of a word, starting with the first bit of a unit.
C
      DO 101 I=1,MXUN(IPSS)
        NDGU=NDGU+1
        NRGT=NBPW-IFBT
        IDGU=IAND(ISHIFT(IDDA(IDDI),NBPU-NRGT),MASK)
        IF (NRGT.LT.NBPU)
     +    IDGU=IOR(ISHIFT(ISHIFT(IDGU,NRGT-NBPU),NBPU-NRGT),
     +          ISHIFT(IAND(ISHIFT(IDDA(IDDI+1),NBPU-NBPW),MASK),-NRGT))
        IF (IDGU.EQ.0) THEN
          RDGU(I)=-2048.
        ELSE
          RDGU(I)=REAL(IDGU-IOFF)
        END IF
        IF (I.NE.1) THEN
          IF (RDGU(I).EQ.-2048..AND.RDGU(I-1).EQ.-2048.) GO TO 102
        END IF
        IFBT=IFBT+NBPU
        IF (IFBT.GE.NBPW) THEN
          IDDI=IDDI+1
          IFBT=IFBT-NBPW
        END IF
  101 CONTINUE
C
C Compensate for the fact that the Y coordinates are off by a little.
C (This error dates from the days of PWRITX.)
C
  102 DO 103 I=4,NDGU,2
        IF (RDGU(I-1).NE.-2048.) THEN
          RDGU(I)=RDGU(I)-1.5
        ELSE
          RDGU(I)=0.
        END IF
  103 CONTINUE
C
C Done.
C
      RETURN
C
      END
