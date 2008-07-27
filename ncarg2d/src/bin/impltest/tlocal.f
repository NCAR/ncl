C
C	$Id: tlocal.f,v 1.4 2008-07-27 00:59:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM TLOCAL
C
C     Test program for locally implemented support routines
C     IAND, IOR, ISHIFT, GBYTES, SBYTES
C
      INTEGER BYTLNG,WRDLNG
      DIMENSION MASK0(64)
      DIMENSION MASK1(65)
      DIMENSION ISCR(190)
C
C     This is a test package designed to test the locally
C     implemented routines for the NCAR GKS-based graphics
C     package.   Although the tests are not exhaustive,
C     they give a fairly  good idea whether a routine works
C     or not.
C
C     Appropriate messages are printed when a routine passes or
C     fails a test.  The test package is written in FORTRAN 77
C     with the following machine assumptions:  non-negative
C     integers must be stored in binary format, in one machine word--
C     hence, a word of all 0 bits is zero; a word with only the
C     rightmost bit set is one, etc.
C
C     The test package consists of a main program and various
C     subroutines.  To run the test package, compile it as FORTRAN,
C     and load and execute it with your locally written routines.
C     Certain information must be supplied to the main program
C     before the test.  This is most conveniently supplied by
C     changing certain data statements at the beginning of the main
C     program.  Below are 8 groups of comments.  Each group
C     describes the information required in the data statements
C     which follow it.  Read the directions and modify the data
C     statements as required.
C
C
C 1.  IOUT.  IOUT is an integer variable containing the unit
C     number on which the test package is to output all messages.
C     It is used, for example, in statements like
C
C                  WRITE (IOUT,9999)
C
C     IOUT has been initialized to 6, but you should reset IOUT so
C     that messages come to your line printer.  The test
C     assumes at least 72 columns per line including carriage
C     control.  There are no read statements.
C
      DATA IOUT/6/
C
C
C 2.  WRDLNG.     WRDLNG is an integer variable which contains the
C     number of bits in a machine word in your computer,
C        e.g.   For an ibm 360/370,  wrdlng = 32;
C               for a CRAY-1,  WRDLNG = 64 .
C     WRDLNG must be at least 16 bits and at most 64 bits.
C
      DATA WRDLNG/0/
C
C
C 3.  INLNGA and INLNGC.
C
C     INLNGA is an integer variable specifying the number of
C     contiguous low-order bits transferred from J to I by the
C     FORTRAN assignment statement
C
C         I=J
C
C     INLNGC is an integer variable specifying the number of
C     contiguous low-order bits actually participating in the
C     comparison performed by a FORTRAN statement like
C
C          IF (I.NE.J) GO TO 10
C
C     These parameters are used to determine the highest-order
C     bit that the test package can use in integer operations.
C     If either is less that WRDLNG, the routines IAND, IOR, and
C     ISHIFT are not fully tested.  If either is less than 16,
C     no testing can be done, and the package probably cannot be
C     made to run.
C
C
      DATA INLNGA /0/
      DATA INLNGC /0/
C
C
C 4.  MASK0.  MASK0 is an integer array of WRDLNG elements.
C     Each element contains all zero bits except for 1 one bit.
C     In element I, the one bit is I-1 bits from the right.  Hence,
C     MASK0(1) has the rightmost bit set, MASK0(WRDLNG) has the
C     leftmost bit set.  In terms of integers, MASK0(I) = 2**(I-1).
C     The test program comes with 64 data statements initializing
C     MASK0.  However, on most computers overflow will cause some of
C     our supplied values to be invalid.  You should begin by
C     removing the last 64-WRDLNG data statements, since these are for
C     word sizes larger than yours.  There should be WRDLNG data
C     statements remaining.  You should then change the last few of
C     these data statements to eliminate overflow, and generate the
C     proper values as defined above.  You may want to use octal or
C     hex constants.  The test program cannot check that MASK0 has
C     been initialized properly, but it is crucial that it be set
C     correctly.
C
C
      DATA MASK0( 1) /                    1 / ,
     1     MASK0( 2) /                    2 / ,
     2     MASK0( 3) /                    4 / ,
     3     MASK0( 4) /                    8 / ,
     4     MASK0( 5) /                   16 / ,
     5     MASK0( 6) /                   32 / ,
     6     MASK0( 7) /                   64 / ,
     7     MASK0( 8) /                  128 / ,
     8     MASK0( 9) /                  256 / ,
     9     MASK0(10) /                  512 / ,
     +     MASK0(11) /                 1024 / ,
     1     MASK0(12) /                 2048 / ,
     2     MASK0(13) /                 4096 / ,
     3     MASK0(14) /                 8192 / ,
     4     MASK0(15) /                16384 / ,
     5     MASK0(16) /                32768 /
      DATA MASK0(17) /                65536 / ,
     1     MASK0(18) /               131072 / ,
     2     MASK0(19) /               262144 / ,
     3     MASK0(20) /               524288 / ,
     4     MASK0(21) /              1048576 / ,
     5     MASK0(22) /              2097152 / ,
     6     MASK0(23) /              4194304 / ,
     7     MASK0(24) /              8388608 / ,
     8     MASK0(25) /             16777216 / ,
     9     MASK0(26) /             33554432 / ,
     +     MASK0(27) /             67108864 / ,
     1     MASK0(28) /            134217728 / ,
     2     MASK0(29) /            268435456 / ,
     3     MASK0(30) /            536870912 / ,
     4     MASK0(31) /           1073741824 / ,
     5     MASK0(32) /           2147483648 /
      DATA MASK0(33) /           4294967296 / ,
     1     MASK0(34) /           8589934592 / ,
     2     MASK0(35) /          17179869184 / ,
     3     MASK0(36) /          34359738368 / ,
     4     MASK0(37) /          68719476736 / ,
     5     MASK0(38) /         137438953472 / ,
     6     MASK0(39) /         274877906944 / ,
     7     MASK0(40) /         549755813888 / ,
     8     MASK0(41) /        1099511627776 / ,
     9     MASK0(42) /        2199023255552 / ,
     +     MASK0(43) /        4398046511104 / ,
     1     MASK0(44) /        8796093022208 / ,
     2     MASK0(45) /       17592186044416 / ,
     3     MASK0(46) /       35184372088832 / ,
     4     MASK0(47) /       70368744177664 / ,
     5     MASK0(48) /      140737488355328 /
      DATA MASK0(49) /      281474976710656 / ,
     1     MASK0(50) /      562949953421312 / ,
     2     MASK0(51) /     1125899906842624 / ,
     3     MASK0(52) /     2251799813685248 / ,
     4     MASK0(53) /     4503599627370496 / ,
     5     MASK0(54) /     9007199254740992 / ,
     6     MASK0(55) /    18014398509481984 / ,
     7     MASK0(56) /    36028797018963968 / ,
     8     MASK0(57) /    72057594037927936 / ,
     9     MASK0(58) /   144115188075855872 / ,
     +     MASK0(59) /   288230376151711744 / ,
     1     MASK0(60) /   576460752303423488 / ,
     2     MASK0(61) /  1152921504606846976 / ,
     3     MASK0(62) /  2305843009213693952 / ,
     4     MASK0(63) /  4611686018427387904 / ,
     5     MASK0(64) /  9223372036854775808 /
C
C
C 5.  MSK0DM.  MSK0DM is an integer variable containing the
C     dimension of MASK0.  It must be at least WRDLNG.  The
C     dimension statement for MASK0 is at the beginning of the main
C     program.  You may leave it unchanged, or if you are cramped
C     for space, lower it to WRDLNG.
C
C
      DATA MSK0DM /64/
C
C 6.  MASK1.  MASK1 is an integer array of WRDLNG+1 elements.
C     Element I has the lower I-1 bits set, the other bits are
C     zero, hence, MASK1(1) = 0, MASK1(2) =1,
C                  MASK1(3) = 3, ..., MASK1(WRDLNG+1) is all ones.
C     In terms of integers MASK1(I) = (2**(I-1))-1 .
C     The test program comes with 65 data statements initializing
C     MASK1.  However on most computers overflow will cause some of
C     these to be invalid.  Begin by removing the last 64-WRDLNG
C     data statements.  You should have WRDLNG+1 remaining data
C     statements.  Change the last few of these to eliminate
C     overflow and generate the values as defined above.  You may
C     want to use octal or hex constants.  It is crucial that
C     MASK1 be initialized properly, even though the test program
C     cannot check for this.
C     WARNING.  On one's complement machines, MASK1(WRDLNG+1) = -0.
C
C
C
      DATA MASK1( 1) /                    0 / ,
     1     MASK1( 2) /                    1 / ,
     2     MASK1( 3) /                    3 / ,
     3     MASK1( 4) /                    7 / ,
     4     MASK1( 5) /                   15 / ,
     5     MASK1( 6) /                   31 / ,
     6     MASK1( 7) /                   63 / ,
     7     MASK1( 8) /                  127 / ,
     8     MASK1( 9) /                  255 / ,
     9     MASK1(10) /                  511 / ,
     +     MASK1(11) /                 1023 / ,
     1     MASK1(12) /                 2047 / ,
     2     MASK1(13) /                 4095 / ,
     3     MASK1(14) /                 8191 / ,
     4     MASK1(15) /                16383 / ,
     5     MASK1(16) /                32767 /
      DATA MASK1(17) /                65535 / ,
     1     MASK1(18) /               131071 / ,
     2     MASK1(19) /               262143 / ,
     3     MASK1(20) /               524287 / ,
     4     MASK1(21) /              1048575 / ,
     5     MASK1(22) /              2097151 / ,
     6     MASK1(23) /              4194303 / ,
     7     MASK1(24) /              8388607 / ,
     8     MASK1(25) /             16777215 / ,
     9     MASK1(26) /             33554431 / ,
     +     MASK1(27) /             67108863 / ,
     1     MASK1(28) /            134217727 / ,
     2     MASK1(29) /            268435455 / ,
     3     MASK1(30) /            536870911 / ,
     4     MASK1(31) /           1073741823 / ,
     5     MASK1(32) /           2147483647 /
      DATA MASK1(33) /           4294967295 / ,
     1     MASK1(34) /           8589934591 / ,
     2     MASK1(35) /          17179869183 / ,
     3     MASK1(36) /          34359738367 / ,
     4     MASK1(37) /          68719476735 / ,
     5     MASK1(38) /         137438953471 / ,
     6     MASK1(39) /         274877906943 / ,
     7     MASK1(40) /         549755813887 / ,
     8     MASK1(41) /        1099511627775 / ,
     9     MASK1(42) /        2199023255551 / ,
     +     MASK1(43) /        4398046511103 / ,
     1     MASK1(44) /        8796093022207 / ,
     2     MASK1(45) /       17592186044415 / ,
     3     MASK1(46) /       35184372088831 / ,
     4     MASK1(47) /       70368744177663 / ,
     5     MASK1(48) /      140737488355327 /
      DATA MASK1(49) /      281474976710655 / ,
     1     MASK1(50) /      562949953421311 / ,
     2     MASK1(51) /     1125899906842623 / ,
     3     MASK1(52) /     2251799813685247 / ,
     4     MASK1(53) /     4503599627370495 / ,
     5     MASK1(54) /     9007199254740991 / ,
     6     MASK1(55) /    18014398509481983 / ,
     7     MASK1(56) /    36028797018963967 / ,
     8     MASK1(57) /    72057594037927935 / ,
     9     MASK1(58) /   144115188075855871 / ,
     +     MASK1(59) /   288230376151711743 / ,
     1     MASK1(60) /   576460752303423487 / ,
     2     MASK1(61) /  1152921504606846975 / ,
     3     MASK1(62) /  2305843009213693951 / ,
     4     MASK1(63) /  4611686018427387903 / ,
     5     MASK1(64) /  9223372036854775807 /
      DATA MASK1(65) / 18446744073709551615 /
C
C 7.  MSK1DM.  MSK1DM is an integer variable containing the
C     dimension of MASK1.  It must be at least WRDLNG+1.  The
C     dimension statement for MASK1 is at the beginning of the main
C     program.  You may leave it unchanged, or if you are cramped
C     for space, lower it to WRDLNG+1.
C
      DATA MSK1DM /65/
C
C
C     ISCRDM.  ISCRDM is an integer variable containing the
C     dimension of ISCR.  You may ignore this step unless you are
C     cramped for space.  ISCR is an array used for scratch.  Its
C     length depends on the word length of your machine.  The test
C     package comes with ISCR dimensioned to 190 .  This is large
C     enough for all word sizes of 1-64 bits, but if you want to
C     lower it, the following formula gives the minimum required.
C     Let LCM stand for the least common multiple.  Then the
C     dimension of ISCR must be at least
C          2*( LCM(WRDLNG,16)/16 + 2*LCM(WRDLNG,16)/WRDLNG ) .
C
      DATA ISCRDM /190/
C
C
C
C     End of information that you are required to supply.
C
C
C
      DATA BYTLNG /16/
      WRITE (IOUT,112)
      WRITE (IOUT,113)
      WRITE (IOUT,114) WRDLNG,INLNGA,INLNGC,MSK0DM,MSK1DM,ISCRDM
      IER=0
      INTLNG=MIN0(INLNGC,INLNGA)
      IF (WRDLNG.LT.BYTLNG .OR. WRDLNG.GT.64) GOTO 107
  101 WRDLNG=MAX0(WRDLNG,BYTLNG)
  102 IF (INLNGA.LT.BYTLNG .OR. INLNGA.GT.WRDLNG) GO TO 108
      IF (INLNGC.LT.BYTLNG .OR. INLNGC.GT.WRDLNG) GO TO 108
  103 IF (MSK1DM.LE.WRDLNG) GOTO 109
  104 IF (MSK0DM.LT.WRDLNG) GOTO 110
  105 NMBRB=2*LCM(WRDLNG,BYTLNG)/BYTLNG
      LPAKD=NMBRB*BYTLNG/WRDLNG
      NEEDED=NMBRB+2*LPAKD
      IF (ISCRDM.LT.NEEDED) GOTO 111
  106 IF (IER.NE.0) STOP
      WRITE (IOUT,115)
      CALL TIOR(MASK1,MSK1DM,WRDLNG,INTLNG,IOUT,IER)
      IERIOR=IER
      CALL TIAND(MASK1,MSK1DM,WRDLNG,INTLNG,IOUT,IER)
      IERAND=IER
      CALL TSHIFT (MASK0,MSK0DM,MASK1(2),MSK1DM-1,IERAND,WRDLNG,INTLNG,
     1                                                         IOUT,IER)
      IERSHF=IER
      CALL TGBYTE (MASK0,MSK0DM,MASK1,MSK1DM,ISCR,ISCRDM,WRDLNG,INTLNG,
     1                                           IERAND,IERSHF,IOUT,IER)
      IERGBY=IER
      CALL TSBYTE (MASK0,MSK0DM,MASK1,MSK1DM,ISCR,ISCRDM,WRDLNG,INTLNG,
     1                                   IERAND,IERIOR,IERSHF,IOUT,IER)
      IERSBY=IER
      WRITE (IOUT,116) IERIOR,IERAND,IERSHF,IERGBY,IERSBY
      STOP
  107 WRITE (IOUT,117) BYTLNG
      IER=1
      GOTO 101
  108 WRITE (IOUT,118) BYTLNG
      IER=1
      GOTO 103
  109 WRITE (IOUT,119)
      IER=1
      GOTO 104
  110 WRITE (IOUT,120)
      IER=1
       GOTO 105
  111 WRITE (IOUT,121) NEEDED
      IER=1
       GOTO 106
  112 FORMAT ('1TEST PACKAGE EXECUTING.  THIS LINE SHOULD BE PRINTED.')
  113 FORMAT ('0WE WILL FIRST TEST THE INFORMATION YOU HAVE PROVIDED',
     1        ' IN CERTAIN DATA'/
     2        ' STATEMENTS AT THE BEGINNING OF THIS PROGRAM.  IF ANY',
     3        ' ERROR IS FOUND,'/
     4        ' EXECUTION WILL TERMINATE--NO ROUTINES WILL BE TESTED.')
  114 FORMAT ('0YOU HAVE PROVIDED THE FOLLOWING CONSTANTS.'/' '/
     1        6X,'NUMBER OF BITS IN A WORD                ',I21/
     2        6X,'NUMBER OF BITS IN AN INTEGER ASSIGNMENT ',I21/
     3        6X,'NUMBER OF BITS IN AN INTEGER COMPARISON ',I21/
     4        6X,'THE DIMENSION OF THE ARRAY MASK0        ',I21/
     5        6X,'THE DIMENSION OF THE ARRAY MASK1        ',I21/
     6        6X,'THE DIMENSION OF THE ARRAY ISCR         ',I21)
  115 FORMAT ('0THE ABOVE DATA APPEARS TO BE CORRECT.  WE WILL NOW',
     1        ' BEGIN TESTING THE'/
     2        ' SUPPORT ROUTINES YOU HAVE PROVIDED FOR USE WITH THE',
     3        ' PLOT PACKAGE.'/
     4        '0A MESSAGE WILL BE PRINTED BEFORE EACH TEST BEGINS,',
     5        ' WHENEVER AN ERROR IS'/
     6        ' FOUND, AND AT THE SUCCESSFUL CONCLUSION OF A TEST. ',
     7        ' A SUMMARY OF THE'/
     8        ' TEST RESULTS WILL BE PRINTED AT THE END.  NOTE THAT',
     9        ' AN ERROR FOUND IN'/
     +        ' ONE ROUTINE MAY SUSPEND TESTING OF ONE OR MORE',
     1        ' SUBSEQUENT ROUTINES.')
  116 FORMAT ('1FOLLOWING IS A SUMMARY OF THE TEST RESULTS SO FAR. ',
     1        ' THE INTEGER WHICH'/
     2        ' FOLLOWS THE ROUTINE NAME IS A COUNT OF ERRORS FOUND',
     3        ' WHILE TESTING THE'/
     4        ' ROUTINE.  (-1 INDICATES THAT THE ROUTINE WAS NOT',
     5        ' TESTED AT ALL.)'/' '/
     6        10X,'IOR       ',I2/
     7        10X,'IAND      ',I2/
     8        10X,'ISHIFT    ',I2/
     9        10X,'GBYTES    ',I2/
     +        10X,'SBYTES    ',I2/
     1        '0IN ANY CASE, ALL PRECEDING OUTPUT SHOULD BE CAREFULLY',
     2        ' EXAMINED.')
  117 FORMAT ('0ERROR.  THE SYSTEM PLOT PACKAGE NEEDS AT LEAST ',I2,
     1     ' BITS IN A WORD.'/
     2     ' THE TEST PACKAGE IS DESIGNED FOR A 64 BITS/WORD MAXIMUM.')
  118 FORMAT ('0ERROR.  THE NUMBER OF LOW ORDER BITS TAKING PART IN ',
     1     'INTEGER COMPARISONS'/
     2     ' AND ASSIGNMENTS MUST BE .LE. WORD SIZE AND .GE. ',I2,'.')
  119 FORMAT ('0ERROR.  THE NUMBER OF ONES MASKS MUST BE .GT. ',
     1     'THE WORD LENGTH.  KEEP'/
     2     ' IN MIND THAT THE KTH ELEMENT HAS K-1 CONTIGUOUS ONES IN ',
     3     'THE LOW ORDER'/
     4     ' BITS.  HENCE MASK1(1) SHOULD BE ZERO, MASK1(WRDLNG+1) ',
     5     'SHOULD BE ALL'/
     6     ' ONES.')
  120 FORMAT('0ERROR.  THE NUMBER OF ONE-WITH-ZEROES MASKS MUST ',
     1     'BE .GE. THE WORD'/
     2     ' LENGTH.  KEEP IN MIND THAT THE KTH ELEMENT HAS BIT K-1 ',
     3     'SET (COUNTING'/
     4     ' BIT 0 AS THE RIGHTMOST) WITH THE REST ZEROS.  HENCE ',
     5     'MASK0(1) SHOULD BE'/
     6     ' 1, AND MASK0(WRDLNG) SHOULD HAVE A ONE IN THE LEFTMOST ',
     7     'BIT, AND THE'/
     8     ' REST ZEROS.')
  121 FORMAT ('0ERROR.  WITH THE GIVEN WORD SIZE, THE SCRATCH ARRAY ',
     1     'MUST BE AT LEAST'/
     2     1X,I16,' ELEMENTS LONG.')
      END
