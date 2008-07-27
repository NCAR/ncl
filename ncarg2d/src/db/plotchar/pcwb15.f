C
C $Id: pcwb15.f,v 1.4 2008-07-27 01:04:31 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE PCWB15 (INUN,IOUT,NOIN,ITMP,LTMP)
C
      DIMENSION ITMP(LTMP)
C
C This routine packs a set of 15-bit integers into an integer array and
C writes out the contents of that array.  Each word of the array holds
C only as many of the 15-bit integers as will fit in one word; they are
C left-justified and there may be some unused zero bits on the end.
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
C   LTMP must be (NOIN*16-1)/NPPW+1 or bigger, where NPPW is the number
C   of complete 15-bit units which will fit into a word.
C
C On exit:
C
C   The values of INUN, IOUT, and NOIN are unchanged.  The unit INUN
C   has been rewound, but is otherwise unchanged.  ITMP contains the
C   packed integers from INUN, NPPW per word, left-justified.  A
C   binary record has been written to IOUT, consisting of the first
C   (NOIN*16-1)/NPPW+1 words of the array ITMP.
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
C Initialize NWDS and NPAR to say that the last parcel position filled
C was position NPPW in word 0, so that the next one to be filled will
C be position 1 (the leftmost) in word 1.
C
      NWDS=0
      NPAR=NPPW
C
C Read integers from unit INUN and store them in the array ITMP.  The
C index I loops through the card images and the index J loops through
C the integers on each card image.
C
      DO 102 I=1,NOIN
        READ (INUN,'(16I5)') ICRD
        DO 101 J=1,16
          NPAR=NPAR+1
          IF (NPAR.GT.NPPW) THEN
            NWDS=NWDS+1
            NPAR=1
            ITMP(NWDS)=ISHIFT(IAND(ICRD(J),32767),NBPW-15)
          ELSE
            ITMP(NWDS)=IOR(ITMP(NWDS),
     +                     ISHIFT(IAND(ICRD(J),32767),NBPW-NPAR*15))
          END IF
  101   CONTINUE
  102 CONTINUE
C
C Write the integer array into the binary file.
C
      CALL NGWRIN (IOUT,ITMP,NWDS,ISTA)
C
      IF (ISTA.NE.NWDS) THEN
        PRINT * , 'IN PCWB15, ERROR WRITING OUTPUT RECORD'
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
