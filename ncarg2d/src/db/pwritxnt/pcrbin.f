C
C	$Id: pcrbin.f,v 1.5 2008-07-27 01:04:31 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      PROGRAM PCRBIN
C
C Because of the difficulty of porting a binary file, the database
C for the PWRITX package is supplied on the distribution tape in
C the four card-image files
C
C   PWRITXC1
C   PWRITXC2
C   PWRITXD1
C   PWRITXD2
C
C PWRITX  uses a binary file as its database.  This program (PCRBIN),
C converts the four card-image files mentioned above into one binary
C file having four records.
C
C After  PCRBIN  has created the desired binary file, it calls two
C short test routines to make sure that accessing of all records
C works correctly.  A message is printed as to the success or
C failure of the test routines.
C
C This program uses the support routines ISHIFT, IAND, and IOR which
C are required locally-implemented support routines for the NCAR
C GKS package.  Also, the entry SETER of the error package ERPRT77
C is invoked.
C
C Before executing PCRBIN the implementation-dependent canstants in
C the BLOCKDATA DPORTX must be set.  Also, make the four files
C PWRITXC1, PWRITXC2, PWRITXD1, and PWRITXD2 available on the
C units specified in BLOCKDATA DPORTX.  Ready the unit specified
C in BLOCKDATA DPORTX to receive the created binary output file
C (the file which will ultimately be used by PWRITX.)
C
C **********************************************************************
C
C          D E C L A R A T I O N S
C
C **********************************************************************
C
C Note that the sizes of IDD, IND, and IWORK may have to be modified
C to contain the number of elements equal to the values of IDDLEN and
C INDLEN as computed in the code below.
C
      COMMON/PWRC0/IDD(8626),IND(789)
      COMMON/IDC0/LENWOR,IWORK(8626)
C
C FOR INITIALIZATION OF CONSTANTS IN CREBIN
C
      COMMON/IDC2/MASK15(15)
C
C FOR INITIALIZATION OF CONSTANTS IN CREBIN AND CREB15
C
      COMMON/IDC1/NBWD,IZERO,MA15
C
C FOR INITIALIZATION OF MACHINE DEPENDENT CONSTANTS
C
      COMMON/PINIT2/FLAG
      LOGICAL FLAG
C
C CONSTANTS DEFINED IN BLOCK DATA DPORTX.
C
      COMMON /IDC3/ IU1,IU2,IU3,IU4,IOUT,ICNUM1,ICNUM2,IDDLEN,INDLEN
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL DPORT
C
C CHECK IF IMPLEMENTATION DEPENDENT CONSTANTS ARE INITIALIZED.
C
      IF (IU1.NE.0) GO TO 1
      CALL SETER(' PCRBIN - MACHINE DEPENDENT CONSTANSTS NOT SET',1,2)
    1 CONTINUE
C
C Open all required datasets.
C
      OPEN (UNIT= IU1,STATUS='OLD',FILE= 'pwritxc1',ERR=901)
      OPEN (UNIT= IU2,STATUS='OLD',FILE= 'pwritxc2',ERR=902)
      OPEN (UNIT= IU3,STATUS='OLD',FILE= 'pwritxd1',ERR=903)
      OPEN (UNIT= IU4,STATUS='OLD',FILE= 'pwritxd2',ERR=904)
      OPEN (UNIT=IOUT,STATUS='NEW',FILE='pwritdata',ERR=905,
     +                                   FORM='UNFORMATTED')
      GO TO 906
  901 PRINT * , 'ERROR OPENING INPUT UNIT (pwritxc1).'
      STOP
  902 PRINT * , 'ERROR OPENING INPUT UNIT (pwritxc2).'
      STOP
  903 PRINT * , 'ERROR OPENING INPUT UNIT (pwritxd1).'
      STOP
  904 PRINT * , 'ERROR OPENING INPUT UNIT (pwritxd2).'
      STOP
  905 PRINT * , 'ERROR OPENING OUTPUT UNIT (pwritdata).'
      STOP
  906 CONTINUE
C
C
C            INITIALIZATION  PASS
C
C CHECK IF DONE
C
      IF (FLAG) GO TO 9000
      FLAG = .TRUE.
C
C GET NUMBER OF BITS PER WORD
C
      NBWD = I1MACH(5)
C
C FIND THE NUMBER OF 15 BIT PARCELS PER WORD
C
      NUM15 = NBWD/15
C
C CALCULATE THE LENGTH OF ARRAYS IWORK,IDD,IND REQUIRED ON THIS
C MACHINE.
C
      LENWOR = (ICNUM2*16*15-1)/NBWD +1
      IDDLEN = LENWOR
      INDLEN = (ICNUM1*16-1)/NUM15+1
C
C GENERATE MASKS FOR THE LEFTMOST 1-15 BITS IN A WORD
C
      MASK = 1
      MASK = ISHIFT(MASK,(NBWD-1))
      MASK15(1) = 0
      DO 1000 I = 2,15
      MASK15(I) = ISHIFT(MASK15(I-1),-1)
      MASK15(I) = IOR(MASK,MASK15(I))
 1000 CONTINUE
C
C
C  END INITIALIZATION
C
C
 9000 CONTINUE
C
C
C
C FOR DIGITIZATION OF COMPLEX CHARACTER SET.
C
C
C CREATE 1 BINARY RECORD ON FILE IOUT CONTAINING THE CONTENTS OF THE
C ARRAY IND IN PWRITX.
C
      CALL CREB15(IU1,IOUT,ICNUM1,IWORK,LENWOR)
C
C
C CREATE 1 BINARY RECORD ON FILE IOUT CONTAINING THE CONTENTS OF THE
C ARRAY IDD IN PWRITX.
C
      CALL CREBIN(IU2,IOUT,ICNUM2,IWORK,LENWOR)
C
C
C FOR DIGITIZATION OF DUPLEX CHARACTER SET.
C
C
C CREATE 1 BINARY RECORD ON FILE IOUT CONTAINING THE CONTENTS OF THE
C ARRAY IND IN PWRITX.
C
      CALL CREB15(IU3,IOUT,ICNUM1,IWORK,LENWOR)
C
C
C CREATE 1 BINARY RECORD ON FILE IOUT CONTAINING THE CONTENTS OF THE
C ARRAY IDD IN PWRITX.
C
      CALL CREBIN(IU4,IOUT,ICNUM2,IWORK,LENWOR)
C
      WRITE(I1MACH(2),101)
 101  FORMAT (1X,' PCRBIN - BINARY FILE CREATED')
C
C Close the output unit and reopen it for input.  It should not be
C necessary to do this, but UNICOS I/O had a problem with just
C rewinding the unit and reading it.
C
      CLOSE (UNIT=IOUT)
C
      OPEN (UNIT=IOUT,STATUS='OLD',FILE='pwritdata',ERR=907,
     +                                   FORM='UNFORMATTED')
      GO TO 908
  907 PRINT * , 'ERROR OPENING INPUT UNIT (pwritdata).'
      STOP
  908 CONTINUE
C
C
C READ THE 2 RECORDS REPRESENTING THE COMPLEX CHARACTER SET.
C
      REWIND IOUT
      READ(IOUT) (IND(I),I=1,INDLEN)
      READ(IOUT) (IDD(I),I=1,IDDLEN)
      REWIND IOUT
C
C TEST THE ACCESSING OF THE COMPLEX CHARACTER SET.
C
      CALL CCHECK(IERR)
C
C PRINT THE TEST RESULTS.
C
      WRITE(I1MACH(2),102)
 102  FORMAT(1X,' PCRBIN - TEST COMPLEX SET')
C
      IF (IERR .NE. 0) GOTO 2
      WRITE(I1MACH(2),103)
 103  FORMAT(1X,' PCRBIN - TEST WAS SUCCESSFUL')
      GOTO 10
C
    2 CONTINUE
      IF (IERR .NE. 1) GOTO 3
      WRITE(I1MACH(2),104)
 104  FORMAT(1X,' TEST SUCCESSFUL FOR 12 BIT BUT NOT FOR 6 BIT UNITS')
      GOTO 10
C
    3 CONTINUE
      IF (IERR .NE. 2) GOTO 4
      WRITE(I1MACH(2),105)
 105  FORMAT(1X,' TEST SUCCESSFUL FOR 6 BIT BUT NOT FOR 12 BIT UNITS')
      GOTO 10
C
    4 CONTINUE
      WRITE(I1MACH(2),106)
 106  FORMAT(1X,' PCRBIN - TEST NOT SUCCESSFUL')
C
C
   10 CONTINUE
C
C
C LOAD THE 2 RECORDS REPRESENTING THE DUPLEX CHARACTER SET.
C
      REWIND IOUT
      READ(IOUT) DUMREA
      READ(IOUT) DUMREA
      READ(IOUT) (IND(I),I=1,INDLEN)
      READ(IOUT) (IDD(I),I=1,IDDLEN)
      REWIND IOUT
C
C TEST ACCESSING OF DUPLEX CHARACTER SET.
C
      CALL DCHECK(IERR)
C
C PRINT THE TEST RESULTS.
C
      WRITE(I1MACH(2),200)
 200  FORMAT(1X,' PCRBIN - TEST DUPLEX SET')
C
      IF (IERR .NE. 0) GOTO 11
      WRITE(I1MACH(2),201)
 201  FORMAT(1X,' PCRBIN - TEST WAS SUCCESSFUL')
      GOTO 20
C
   11 CONTINUE
      IF (IERR .NE. 1) GOTO 12
      WRITE(I1MACH(2),202)
 202  FORMAT(1X,' TEST SUCCESSFUL FOR 12 BIT BUT NOT FOR 6 BIT UNITS')
      GOTO 20
C
   12 CONTINUE
      IF (IERR .NE. 2) GOTO 13
      WRITE(I1MACH(2),203)
 203  FORMAT(1X,' TEST SUCCESSFUL FOR 6 BIT BUT NOT FOR 12 BIT UNITS')
      GOTO 20
C
   13 CONTINUE
      WRITE(I1MACH(2),204)
 204  FORMAT(1X,' PCRBIN - TEST NOT SUCCESSFUL')
C
C
   20 CONTINUE
C
      STOP
C
      END
