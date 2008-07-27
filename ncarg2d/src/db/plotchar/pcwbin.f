C
C $Id: pcwbin.f,v 1.4 2008-07-27 01:04:31 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE PCWBIN (INUN,IOUT,NOIN,ITMP,LTMP)
C
      DIMENSION ITMP(LTMP)
C
C This routine packs a set of 15-bit integers into an integer array and
C writes out the contents of that array.  The 15-bit integers are packed
C in such a way as to form a continuous bit stream; there are no unused
C bits, except perhaps in the last word of the array.
C
C On entry:
C
C   INUN is the unit number from which may be read NOIN card images.
C   Each card image contains 16 integers, in I5 format.  Each integer
C   represents a positive 15-bit value.
C
C   IOUT is the file descriptor of a file to which output is to be
C   written.  IOUT is assumed to be positioned correctly.
C
C   NOIN is the number of card images on unit INUN.
C
C   ITMP is an array of length LTMP.  Its elements may have any value.
C
C   LTMP must be (NOIN*15*16-1)/NBPW+1 or bigger, where NBPW is the
C   number of bits per word.
C
C On exit:
C
C   The values of INUN, IOUT, and NOIN are unchanged.  The unit INUN
C   has been rewound, but is otherwise unchanged.  ITMP contains the
C   packed integers from INUN.  A binary record has been written to
C   IOUT, consisting of the first (NOIN*15*16-1)/NBPW+1 words of the
C   array ITMP.
C
C COMMON block declarations.
C
      COMMON /PCCMN2/ NBPW,NPPW
C
C Define temporary storage for a card image.
C
      DIMENSION ICRD(16)
C
C Rewind the input unit.
C
      REWIND INUN
C
C Initialize NBIT and NWDS to say that the last position filled was the
C last fifteen bits in word 0, so that the next one to be filled will
C be the first fifteen bits in word 1.
C
      NBIT=NBPW-15
      NWDS=0
C
C Read integers from unit INUN and store them in the array ITMP.  The
C index I loops through the card images and the index J loops through
C the integers on each card image.
C
      DO 102 I=1,NOIN
        READ (INUN,'(16I5)') ICRD
        DO 101 J=1,16
          NBIT=NBIT+15
          IF (NBIT.GE.NBPW) THEN
            NBIT=NBIT-NBPW
            NWDS=NWDS+1
            IF (NBIT.EQ.0) ITMP(NWDS)=0
          END IF
          ITMP(NWDS)=IOR(ITMP(NWDS),
     +                   ISHIFT(IAND(ICRD(J),32767),NBPW-15-NBIT))
          IF (NBIT+15.GT.NBPW) THEN
            ITMP(NWDS+1)=ISHIFT(IAND(ICRD(J),ISHIFT(32767,NBIT-NBPW)),
     +                          2*NBPW-NBIT-15)
          END IF
  101   CONTINUE
  102 CONTINUE
C
C If ITMP(NWDS+1) has something in it, bump NWDS.
C
      IF (NBIT+15.GT.NBPW) NWDS=NWDS+1
C
C Write the integer array into the binary file.
C
      CALL NGWRIN (IOUT,ITMP,NWDS,ISTA)
C
      IF (ISTA.NE.NWDS) THEN
        PRINT * , 'IN PCWBIN, ERROR WRITING OUTPUT RECORD'
        STOP
      END IF
C
C Rewind the input unit.
C
      REWIND INUN
C
C Done.
C
      RETURN
C
      END
