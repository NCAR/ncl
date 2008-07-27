C
C	$Id: tlocalibm.f,v 1.4 2008-07-27 00:59:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM TLOCAL
C
C +-----------------------------------------------------------------+
C |                                                                 |
C |                Copyright (C) 1987 by UCAR                       |
C |        University Corporation for Atmospheric Research          |
C |                    All Rights Reserved                          |
C |                                                                 |
C |                 NCARGRAPHICS  Version 2.00                      |
C |                                                                 |
C +-----------------------------------------------------------------+
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

      DATA WRDLNG/32/

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

      DATA INLNGA /32/
      DATA INLNGC /32/


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
      DATA	
     +  MASK0(1)	/	Z00000001/,
     +  MASK0(2)	/	Z00000002/,
     +  MASK0(3)	/	Z00000004/,
     +  MASK0(4)	/	Z00000008/,
     +  MASK0(5)	/	Z00000010/,
     +  MASK0(6)	/	Z00000020/,
     +  MASK0(7)	/	Z00000040/,
     +  MASK0(8)	/	Z00000080/,
     +  MASK0(9)	/	Z00000100/,
     +  MASK0(10)	/	Z00000200/,
     +  MASK0(11)	/	Z00000400/,
     +  MASK0(12)	/	Z00000800/,
     +  MASK0(13)	/	Z00001000/,
     +  MASK0(14)	/	Z00002000/,
     +  MASK0(15)	/	Z00004000/,
     +  MASK0(16)	/	Z00008000/

      DATA	
     +  MASK0(17)	/	Z00010000/,
     +  MASK0(18)	/	Z00020000/,
     +  MASK0(19)	/	Z00040000/,
     +  MASK0(20)	/	Z00080000/,
     +  MASK0(21)	/	Z00100000/,
     +  MASK0(22)	/	Z00200000/,
     +  MASK0(23)	/	Z00400000/,
     +  MASK0(24)	/	Z00800000/,
     +  MASK0(25)	/	Z01000000/,
     +  MASK0(26)	/	Z02000000/,
     +  MASK0(27)	/	Z04000000/,
     +  MASK0(28)	/	Z08000000/,
     +  MASK0(29)	/	Z10000000/,
     +  MASK0(30)	/	Z20000000/,
     +  MASK0(31)	/	Z40000000/,
     +  MASK0(32)	/	Z80000000/
	
C
C
C 5.  MSK0DM.  MSK0DM is an integer variable containing the
C     dimension of MASK0.  It must be at least WRDLNG.  The
C     dimension statement for MASK0 is at the beginning of the main
C     program.  You may leave it unchanged, or if you are cramped
C     for space, lower it to WRDLNG.
C
C

      DATA MSK0DM /32/

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
      DATA	
     +  MASK1(1)	/	Z00000000/,
     +  MASK1(2)	/	Z00000001/,
     +  MASK1(3)	/	Z00000003/,
     +  MASK1(4)	/	Z00000007/,
     +  MASK1(5)	/	Z0000000f/,
     +  MASK1(6)	/	Z0000001f/,
     +  MASK1(7)	/	Z0000003f/,
     +  MASK1(8)	/	Z0000007f/,
     +  MASK1(9)	/	Z000000ff/,
     +  MASK1(10)	/	Z000001ff/,
     +  MASK1(11)	/	Z000003ff/,
     +  MASK1(12)	/	Z000007ff/,
     +  MASK1(13)	/	Z00000fff/,
     +  MASK1(14)	/	Z00001fff/,
     +  MASK1(15)	/	Z00003fff/,
     +  MASK1(16)	/	Z00007fff/

      DATA
     +  MASK1(17)	/	Z0000ffff/,
     +  MASK1(18)	/	Z0001ffff/,
     +  MASK1(19)	/	Z0003ffff/,
     +  MASK1(20)	/	Z0007ffff/,
     +  MASK1(21)	/	Z000fffff/,
     +  MASK1(22)	/	Z001fffff/,
     +  MASK1(23)	/	Z003fffff/,
     +  MASK1(24)	/	Z007fffff/,
     +  MASK1(25)	/	Z00ffffff/,
     +  MASK1(26)	/	Z01ffffff/,
     +  MASK1(27)	/	Z03ffffff/,
     +  MASK1(28)	/	Z07ffffff/,
     +  MASK1(29)	/	Z0fffffff/,
     +  MASK1(30)	/	Z1fffffff/,
     +  MASK1(31)	/	Z3fffffff/,
     +  MASK1(32)	/	Z7fffffff/,
     +  MASK1(33)	/	Zffffffff/

C
C 7.  MSK1DM.  MSK1DM is an integer variable containing the
C     dimension of MASK1.  It must be at least WRDLNG+1.  The
C     dimension statement for MASK1 is at the beginning of the main
C     program.  You may leave it unchanged, or if you are cramped
C     for space, lower it to WRDLNG+1.
C

      DATA MSK1DM /33/

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
