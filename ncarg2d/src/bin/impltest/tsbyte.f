C
C	$Id: tsbyte.f,v 1.4 2008-07-27 00:59:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
       SUBROUTINE TSBYTE(MASK0,MSK0DM,MASK1,MSK1DM,ISCR,ISCRDM,
     +                   WRDLNG,INTLNG,IERAND,IERIOR,IERSHF,IOUT,IER)
       DIMENSION MASK0(MSK0DM),MASK1(MSK1DM),ISCR(ISCRDM)
       INTEGER WRDLNG
C
C Determine whether test can be executed.
C
       IER=-1
       WRITE(IOUT,7)
       IF(WRDLNG.LT.4) THEN
          WRITE(IOUT,8)
          RETURN
       END IF
       IF(IERAND.NE.0 .OR. IERIOR.NE.0 .OR. IERSHF.NE.0) THEN
           WRITE(IOUT,9)
           RETURN
       END IF
C
C Start of test proper.
C
       IER=1
C
C Test one : set the I-th bit in ITEST to 1 where I ranges
C            from 1 to WRDLNG.
C
       IONE = 1
       NBITS = 1
       NSKIP = 0
       ITER = 1
       DO 1 I =1,WRDLNG
          IBIT = WRDLNG-I
          ITEST = 0
          CALL SBYTES(ITEST,IONE,IBIT,NBITS,NSKIP,ITER)
          IF ( ITEST.NE.MASK0(I)) THEN
             WRITE(IOUT,101) I
             RETURN
          END IF
 1     CONTINUE
C
C Test two : set the last INTLNG bits of the Ith word of
C            ISCR to MASK0(I) where I ranges from 1 to INTLG.
C
       DO 2 I =1,ISCRDM
 2     ISCR(I) = 0
       IBIT = WRDLNG - INTLNG
       NBITS = INTLNG
       NSKIP = IBIT
       ITER = INTLNG
       CALL SBYTES(ISCR,MASK0,IBIT,NBITS,NSKIP,ITER)
       DO 3 I =1,INTLNG
          IF (ISCR(I).NE.MASK0(I)) THEN
             WRITE(IOUT,102)
             RETURN
             END IF
 3     CONTINUE
C
C Test three : set the 1st two bits of ISCR(I) and the last two bits
C              of ISCR(I+1). Test the setting of bits crossing word
C              boundary.
C
       DO 4 I =1,ISCRDM
 4     ISCR(I) = 0
       DO 5 I = 1,16
          ISCR(17+I) = I-1
 5     CONTINUE
       IBIT = WRDLNG-2
       NBITS = 4
       NSKIP = WRDLNG-4
       ITER = 16
       CALL SBYTES(ISCR,ISCR(18),IBIT,NBITS,NSKIP,ITER)
       NSKP1 = NSKIP + 1
       NSKM2 = 2 - WRDLNG
       DO 6 I = 1,16
          ISET1 = IAND(ISHIFT(ISCR(I+1),NSKM2),3)
          ISET2 = ISHIFT(IAND(ISCR(I),MASK1(3)),2)
          ISET = IOR(ISET1,ISET2)
          IF(ISET.NE.I-1) THEN
             WRITE(IOUT,103)
             RETURN
          END IF
 6     CONTINUE
       IER = 0
       WRITE(IOUT,104)
       RETURN
C
 7     FORMAT('1NOW TESTING SBYTES.')
 8     FORMAT('0ERROR IN TEST PACKAGE DETECTED IN TSBYTE. THE WORD ',
     1        'LENGTH MUST BE AT',/,
     2        ' LEAST 4.')
 9     FORMAT('0THE ROUTINES IAND, IOR, AND ISHIFT, WHICH ARE ',
     1        'REQUIRED TO TEST SBYTES HAVE',/,' NOT BEEN ',
     2        'VERIFIED. BECAUSE OF THIS, SBYTES CAN NOT BE TESTED.')
 101   FORMAT('0SBYTES DID NOT SUCCESSFULLY SET THE ITH BIT OF ',
     1        'ITEST WHEN I WAS EQUAL TO ',I16,'.')
 102   FORMAT('0SBYTES DID NOT SUCCESSFULLY SET THE LAST INTLNG ',
     1        'BITS IN THE ITH WORD ',/,
     2        'OF ISCR EQUAL TO MASK0(I) WHEN I WAS ',
     3        'EQUAL TO ',I16,'.')
 103   FORMAT('0SBYTES DID NOT SUCCESSFULLY SET A BIT CHUNK ',
     1        'OF LENGTH 4 WHICH',/,
     2        ' CROSSED A WORD BOUNDARY.')
 104   FORMAT('0FINISHED TESTING SBYTES.  SBYTES APPARENTLY WORKS.')
       END
