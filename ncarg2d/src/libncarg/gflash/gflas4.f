C
C	$Id: gflas4.f,v 1.8 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GFLAS4(ID,FNAME)
C
C_BEGIN PROLOGUE GFLAS4
C
C_PURPOSE
C    A call to this subroutine does the necessary bookkeeping
C    to allow a dataset of plotting instructions created in a previous
C    job step with a GFLAS1/GFLAS2 sequence to be used in the
C    current program with a GFLAS3 call.  GFLAS4 can be used
C    only with the NCAR GKS package.
C
C
C_DESCRIPTION
C    A call to this subroutine does the necessary bookkeeping
C    to allow a dataset of plotting instructions created in a previous
C    job step with a GFLAS1/GFLAS2 sequence to be used in the
C    current program with a GFLAS3 call.  This allows for inserting
C    previously-created plotting instructions into the output
C    pipeline at any user-selected point.  GFLAS4 can be used only
C    with the NCAR GKS package.
C
C_ARGUMENTS
C
C ID      (INPUT,SINGLE,VARIABLE)  An integer variable in the range
C         [0,99] which is used as a user-defined identifier for the
C         dataset of graphics instructions contained in FNAME (see
C         below.)  ID can be used as an argument to subsequent
C         GFLAS3 calls.  ID cannot be equal to an identifier
C         used as an argument to GFLAS1 in the current program.
C FNAME   (INPUT,SINGLE,VARIABLE)  A character string containing
C         the dataset name of the dataset which contains plotting
C         instructions generated previously with a GFLAS1/GFLAS2
C         sequence.  FNAME can be at most seven characters in length.
C
C_I/O
C    (None)
C
C_ROUTINES CALLED
C    PLOTIT, SETER
C
C_COMMON BLOCKS
C    GFLASH
C
C_MAXIMUM GKS LEVEL
C    0A
C
C_LANGUAGE
C    FORTRAN
C
C_REVISION DATE
C    890117 (YYMMDD)
C
C_HISTORY
C    This subroutine was coded at NCAR by Fred Clare.  Its purpose
C    is to duplicate the functionality of the previous NCAR System
C    Plot Package entry FLASH4 in a GKS environment.
C
C_REFERENCES
C    "The System Plot Package", NCAR Technical Note number
C    NCAR-TN/162+IA, May, 1983.
C
C    "NCAR Graphics User's Guide", Version 2.00, NCAR Technical Note
C    number NCAR/TN-283+IA, August, 1987.
C
C_LONG DESCRIPTION
C    GFLAS4 finds the number of records in FNAME and does the
C    necessary bookkeeping to establish the dataset as valid for
C    subsequent use by GFLAS3.  GFLAS4 utilizes a GKS ESCAPE
C    function unique to the NCAR GKS package.  The function
C    identifier for this ESCAPE function is -1392.  If one tries
C    to use GFLAS4 with a non-NCAR GKS package, he will get an
C    error to the effect that no ESCAPE function exists with
C    function idendifier -1392.
C
C_EXAMPLES
C    Example 1 --  Establish a previously existing dataset of plotting
C                  instructions -- MF09 -- for use with GFLAS3.
C          SUBROUTINE INSDAT
C          CALL GFLAS4(9,'MF09')
C          CALL GFLAS3(9)
C          RETURN
C          END
C
C_END PROLOGUE GFLAS4
C
C-------------------------------------------------------------
C
C  Common for FLASH package.
C
      COMMON /GFLASH/MODEF,IOPWKS(100),IOACT(100),NUMOP,IWISSI
C
      CHARACTER*(*) FNAME
      CHARACTER*80 IDR(2),ODR(1)
C
      IF (MODEF .EQ. 1) THEN
        GO TO 106
      ELSE
        MODEF = 4
      ENDIF
C
C  Check if GKS is open.
C
      CALL GQOPS(IOPSTA)
      IF (IOPSTA .LT. 1) GO TO 105
C
C  Make sure that this GKS implementation has WISS (level 2).
C
      CALL GQEWK(1,IER,NUMB,NTYP)
      DO 10 I=1,NUMB
        CALL GQEWK(I,IER,NUMB,NTYP)
        CALL GQWKCA(NTYP,IER,NCAT)
        IF (NCAT .EQ. 3) GO TO 20
   10 CONTINUE
      GO TO 102
   20 CONTINUE
C
C  Make sure that WISS is open (IWISSI is initialized to an
C  arbitrary illegal value).
C
      IWISSI = -1335
      CALL GQOPWK(1,IER,IOL,IWKID)
      NUMOP = IOL
      IF (NUMOP .LT. 1) GO TO 103
      IF (NUMOP .GT. 100) GO TO 104
      DO 30 I=1,NUMOP
      CALL GQOPWK(I,IER,IOL,IOPWKS(I))
      IOACT(I) = 0
      CALL GQWKS(IOPWKS(I),IER,IOACT(I))
      CALL GQWKC(IOPWKS(I),IER,ICON,ITYPE)
      CALL GQWKCA(ITYPE,IER,ICAT)
      IF (ICAT .EQ. 3) THEN
        IWISSI = IOPWKS(I)
        CALL GQWKC(IWISSI,IER,ICONW,ITYPE)
      ENDIF
   30 CONTINUE
      IF (IWISSI .EQ. -1335) GO TO 103
C
C  Encode parameters into input data record.
C
      IDR(1) = ' '
      IDR(2) = ' '
      IFID = -1392
      ILEN = LEN(FNAME)
      WRITE(IDR(1)( 1:10),500) IFID
      WRITE(IDR(1)(11:20),500) ID
      WRITE(IDR(1)(21:23),501) ICONW
      IF (ILEN .LE. 57) THEN
        IDR(1)(24:24+ILEN-1) = FNAME(1:ILEN)
      ELSE IF (ILEN .LE. 137) THEN
        IDR(1)(24:24+ILEN-1) = FNAME(1:57)
        IDR(2)(1:ILEN-57) = FNAME(58:ILEN)
      ELSE
        GO TO 107
      ENDIF
      CALL PLOTIT(0,0,2)
      IF (ILEN .LE. 57) THEN
        CALL GESC(IFID,1,IDR,1,IDUM,ODR)
      ELSE
        CALL GESC(IFID,2,IDR,1,IDUM,ODR)
      ENDIF
      CALL PLOTIT(0,0,2)
      RETURN
C
  102 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS4 -- THE NON NCAR GKS PACKAGE IS NOT LEVEL 2',
     -           10,2)
      STOP
  103 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS4 -- WISS MUST BE OPEN BEFORE CALL TO GFLAS4',
     -           11,2)
      STOP
  104 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS4 -- MAXIMUM NUMBER OF OPEN WORKSTATIONS ALLOWED
     - IS 100',12,2)
      STOP
  105 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS4 -- GKS MUST BE OPEN BEFORE A CALL TO GFLAS4',
     -  13,2)
  106 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS4 -- CANNOT BE CALLED WHILE GFLAS1 IS STILL OPEN
     -',14,2)
  107 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS4 -- Maximum length of a file name is 137 charac
     -ters',15,2)
C
  500 FORMAT(I10)
  501 FORMAT(I3)
C
      END
