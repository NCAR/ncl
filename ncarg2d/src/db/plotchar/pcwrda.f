C
C $Id: pcwrda.f,v 1.4 2008-07-27 01:04:31 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      PROGRAM PCWRDA
C
C Because of the difficulty of porting a binary file, the database for
C the PWRITX database is supplied in the distribution in the form of
C four ASCII card-image files named "pwritxc1", "pwritxc2", "pwritxd1",
C and "pwritxd2".  This program reads those files and writes a single
C binary file to be used by PLOTCHAR.
C
C After PCWRDA has created the desired binary file, it calls two short
C test routines to see whether it can be accessed correctly.  Messages
C are printed indicating the success or failure of these test routines.
C
C PCWRDA uses the support routines ISHIFT, IAND, and IOR, which are
C required locally-implemented support routines for NCAR's GKS package.
C
C COMMON block declarations.
C
      COMMON /PCCMN1/ INDA(789),INDL,IDDA(8625),IDDL
      COMMON /PCCMN2/ NBPW,NPPW
      COMMON /PCCMN3/ RDGU(7000)
C
C Declare a variable in which to put the name of the output file.
C
      CHARACTER*13 FLNM
C
C Define the number of "cards" in the files "pwritxc1" and "pwritxd1".
C
      DATA INC1 / 49 /
C
C Define the number of "cards" in the files "pwritxc2" and "pwritxd2".
C
      DATA INC2 / 575 /
C
C Define the numbers of the units from which the files "pwritxc1",
C "pwritxc2", "pwritxd1", and "pwritxd2", respectively, can be read.
C
      DATA IUN1,IUN2,IUN3,IUN4 / 1,2,3,4 /
C
C Log on.
C
      PRINT * , 'Program "WritePlotcharData" executing.'
C
C Open all required files.
C
      OPEN (UNIT=IUN1,STATUS='OLD',FILE='pwritxc1',ERR=901)
      OPEN (UNIT=IUN2,STATUS='OLD',FILE='pwritxc2',ERR=902)
      OPEN (UNIT=IUN3,STATUS='OLD',FILE='pwritxd1',ERR=903)
      OPEN (UNIT=IUN4,STATUS='OLD',FILE='pwritxd2',ERR=904)
C
      FLNM='PlotcharData'//CHAR(0)
      CALL NGOFWO (FLNM,IOUT,ISTA)
C
      IF (ISTA.NE.0) THEN
        PRINT * , 'Error opening file "PlotcharData" for output.'
        STOP
      END IF
C
C Find the number of bits per word.
C
      NBPW=I1MACH(5)
C
C Find the number of 15-bit parcels per word.
C
      NPPW=NBPW/15
C
C Calculate the length of arrays INDA and IDDA required on this
C machine.
C
      INDL=(INC1*16-1)/NPPW+1
      IDDL=(INC2*16*15-1)/NBPW+1
C
C Write two binary records representing the complex character set.
C
      CALL PCWB15 (IUN1,IOUT,INC1,INDA,INDL)
C
      CALL PCWBIN (IUN2,IOUT,INC2,IDDA,IDDL)
C
C Write two binary records representing the duplex character set.
C
      CALL PCWB15 (IUN3,IOUT,INC1,INDA,INDL)
C
      CALL PCWBIN (IUN4,IOUT,INC2,IDDA,IDDL)
C
C Close the output file.
C
      CALL NGCLFI (IOUT)
C
C Log what's happened so far.
C
      PRINT * , 'File "PlotcharData" created.'
C
C Reopen the new file for input.
C
      CALL NGOFRO (FLNM,IOUT,ISTA)
C
      IF (ISTA.NE.0) THEN
        PRINT * , 'Error opening file "PlotcharData" for input.'
        STOP
      END IF
C
C Read the two records of the complex character set.
C
      CALL NGRDIN (IOUT,INDA,INDL,ISTA)
C
      IF (ISTA.NE.INDL) THEN
        PRINT * , 'Error reading file "PlotcharData".'
        STOP
      END IF
C
      CALL NGRDIN (IOUT,IDDA,IDDL,ISTA)
C
      IF (ISTA.NE.IDDL) THEN
        PRINT * , 'Error reading file "PlotcharData".'
        STOP
      END IF
C
C Test accessing of complex character set.
C
      CALL PCCCHK (IERR)
C
C Print test results.
C
      PRINT * , 'Testing complex character set.'
C
      IF (IERR.EQ.0) THEN
        PRINT * , 'Test successful.'
      ELSE IF (IERR.EQ.1) THEN
        PRINT * , 'Test successful for 12-bit but not for 6-bit units.'
      ELSE IF (IERR.EQ.2) THEN
        PRINT * , 'Test successful for 6-bit but not for 12-bit units.'
      ELSE
        PRINT * , 'Test not successful.'
      END IF
C
C Read the two records of the duplex character set.
C
      CALL NGRDIN (IOUT,INDA,INDL,ISTA)
C
      IF (ISTA.NE.INDL) THEN
        PRINT * , 'Error reading file "PlotcharData".'
        STOP
      END IF
C
      CALL NGRDIN (IOUT,IDDA,IDDL,ISTA)
C
      IF (ISTA.NE.IDDL) THEN
        PRINT * , 'Error reading file "PlotcharData".'
        STOP
      END IF
C
C Test accessing of duplex character set.
C
      CALL PCDCHK(IERR)
C
C Print test results.
C
      PRINT * , 'Testing duplex character set.'
C
      IF (IERR.EQ.0) THEN
        PRINT * , 'Test successful.'
      ELSE IF (IERR.EQ.1) THEN
        PRINT * , 'Test successful for 12-bit but not for 6-bit units.'
      ELSE IF (IERR.EQ.2) THEN
        PRINT * , 'Test successful for 6-bit but not for 12-bit units.'
      ELSE
        PRINT * , 'Test not successful.'
      END IF
C
C Done.
C
      STOP
C
C Error exits.
C
  901 PRINT * , 'Error opening file "pwritxc1" for input.'
      STOP
C
  902 PRINT * , 'Error opening file "pwritxc2" for input.'
      STOP
C
  903 PRINT * , 'Error opening file "pwritxd1" for input.'
      STOP
C
  904 PRINT * , 'Error opening file "pwritxd2" for input.'
      STOP
C
      END
