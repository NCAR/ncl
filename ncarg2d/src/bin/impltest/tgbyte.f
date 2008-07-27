C
C	$Id: tgbyte.f,v 1.4 2008-07-27 00:59:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
       SUBROUTINE TGBYTE(MASK0,MSK0DM,MASK1,MSK1DM,ISCR,ISCRDM,
     +                   WRDLNG,INTLNG,IERAND,IERSHF,IOUT,IER)
       INTEGER WRDLNG
       DIMENSION MASK0(MSK0DM),MASK1(MSK1DM),ISCR(ISCRDM)
C
C Determine whether test can be executed.
C
       IER=-1
       WRITE(IOUT,6)
       IF(WRDLNG.LT.4) THEN
          WRITE(IOUT,7)
          RETURN
       END IF
       IF(IERSHF.NE.0 .OR. IERAND.NE.0) THEN
           WRITE(IOUT,8)
           RETURN
       END IF
C
C Start of test proper.
C
       IER=1
C
C Test one : get the I-th bit from MASK0(I) where I ranges
C            from 1 to WRDLNG.
C
       NBITS = 1
       ITER = 1
       NSKIP = 0
       DO 1 I =1,WRDLNG
          IBIT = WRDLNG - I
          CALL GBYTES(MASK0(I),ITEST,IBIT,NBITS,NSKIP,ITER)
          IF ( ITEST.NE.1) THEN
             WRITE(IOUT,101) I
             RETURN
          END IF
 1     CONTINUE
C
C Test two : get the last INTLNG bits in each of the 1st INTLNG words
C            of MASK0.
C
       IBIT = WRDLNG - INTLNG
       NBITS = INTLNG
       NSKIP = IBIT
       ITER = INTLNG
       CALL GBYTES(MASK0,ISCR,IBIT,NBITS,NSKIP,ITER)
       DO 2 I =1,INTLNG
          IF (ISCR(I).NE.MASK0(I)) THEN
             WRITE(IOUT,102) I
             RETURN
             END  IF
 2     CONTINUE
C
C Test three : Get the 1st two bits from ISCR(I) and the last two bits
C              from ISCR(I+1). Test of retrieval of bits crossing word
C              boundary.
C
       DO 3 I =1,ISCRDM
 3     ISCR(I) = 0
       DO 4 I = 1,16
          ISCR(2*I-1) = ISHIFT(I-1,-2)
          ISCR(2*I) = ISHIFT(IAND(I-1,MASK1(3)),WRDLNG-2)
 4     CONTINUE
       IBIT = WRDLNG-2
       NBITS = 4
       NSKIP = 2*WRDLNG-4
       ITER = 16
       CALL GBYTES(ISCR,ISCR(33),IBIT,NBITS,NSKIP,ITER)
       DO 5 I = 1,16
          IF(ISCR(I+32).NE.I-1) THEN
             WRITE(IOUT,103)
             RETURN
          END IF
 5     CONTINUE
       IER = 0
       WRITE(IOUT,104)
       RETURN
C
 6     FORMAT('1NOW TESTING GBYTES.')
 7     FORMAT('0ERROR IN TEST PACKAGE DETECTED IN TGBYTE. THE WORD ',
     1        'LENGTH MUST BE AT',/,
     2        ' LEAST 4.')
 8     FORMAT('0THE ROUTINES IAND AND ISHIFT, WHICH ARE REQUIRED TO ',
     1        'TEST GBYTES HAVE NOT BEEN',/,
     2        ' VERIFIED. BECAUSE OF THIS, GBYTES CAN NOT BE TESTED.')
 101   FORMAT('0GBYTES DID NOT SUCCESSFULLY GET THE ITH BIT OF ',
     1        'MASK0(I) WHEN I WAS EQUAL TO ',I16,'.')
 102   FORMAT('0GBYTES DID NOT SUCCESSFULLY GET THE LAST INTLNG ',
     1        'BITS IN THE ITH WORD',/,
     2        ' OF MASK0 WHEN I WAS EQUAL TO ',I16,'.')
 103   FORMAT('0GBYTES DID NOT SUCCESSFULLY GET A BIT CHUNK ',
     1        'OF LENGTH 4 WHICH',/,
     2        ' CROSSED A WORD BOUNDARY.')
 104   FORMAT('0FINISHED TESTING GBYTES.  GBYTES APPARENTLY WORKS.')
       END
