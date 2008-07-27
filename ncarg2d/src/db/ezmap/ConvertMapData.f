C
C $Id: ConvertMapData.f,v 1.7 2008-07-27 01:04:30 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM CONVRT
C
C This program reads data from the ASCII file "ezmapdat" and writes
C four outline datasets for use by EZMAP.
C
C The data in the file "ezmapdat" are grouped in logical records, each
C of which represents a single outline segment and exists as a pair of
C FORTRAN records.  The first record of each contains four integers and
C four reals.  The second record of each pair contains a varying number
C of real lat/lon coordinate pairs.  In terms of the variables into
C which the data are read:
C
C   INTS(1) = the number of reals in second FORTRAN record, but if
C             the value is 1, there is no second FORTRAN record: this
C             signals the end of a group of data in "ezmapdat".
C   INTS(2) = group identifier for outline segment.
C   INTS(3) = left area identifier for outline segment.
C   INTS(4) = right area identifier for outline segment.
C   FLTS(1) = biggest latitude in outline segment.
C   FLTS(2) = smallest latitude in outline segment.
C   FLTS(3) = biggest longitude in outline segment.
C   FLTS(4) = smallest longitude in outline segment.
C  (FLTS(I),I=5,4+INTS(1)) = lat/lon pairs for outline segment.
C
C Dimension the arrays into which the data are to be read.
C
        DIMENSION INTS(4),FLTS(204)
C
C Set up an equivalent name for the array element specifying the length
C of the second FORTRAN record.
C
        EQUIVALENCE (INTS(1),LOSR)
C
C Declare variables involved in forming names of output files.
C
        CHARACTER*17 FLNM
C
        CHARACTER*2 NOOD(4)
C
C Define the suffixes for the names of the four output datasets.
C
        DATA NOOD / 'CO','US','PS','PO' /
C
C Open the input unit and make sure it's rewound.
C
        OPEN (UNIT=1,STATUS='OLD',FILE='ezmapdat',ERR=901)
        REWIND 1
C
C Initialize the count of output files written so far.
C
        NOUT=0
C
C Open the next output file.
C
  101   NOUT=NOUT+1
        FLNM='EzmapOutlines.'//NOOD(NOUT)//CHAR(0)
        CALL NGOFWO (FLNM,IFDE,ISTA)
        IF (ISTA.NE.0) GO TO 902
C
C Read the next input record and copy the integer part of it to the
C output file.
C
  102   READ (1,'(4I4,4F8.3)',END=903) (INTS(I),I=1,4),(FLTS(I),I=1,4)
C
        CALL NGWRIN (IFDE,INTS,4,ISTA)
        IF (ISTA.NE.4) GO TO 904
C
C If lat/lon data follow, copy them to the output file and go back for
C another data record.  Otherwise, close the output file, and, if it
C wasn't the fourth, go back to open the next.
C
        IF (LOSR.GT.1) THEN
          READ (1,'(10F8.3)',END=903) (FLTS(4+I),I=1,LOSR)
          CALL NGWRFL (IFDE,FLTS,4+LOSR,ISTA)
          IF (ISTA.NE.4+LOSR) GO TO 905
          GO TO 102
        ELSE
          CALL NGCLFI (IFDE)
          IF (NOUT.LT.4) GO TO 101
        END IF
C
C Normal exit.
C
        STOP
C
C Error exits.
C
  901   PRINT * , 'ERROR - Can''t open file "ezmapdat".'
        STOP
C
  902   PRINT * , 'ERROR - NGOFWO returns bad status:',ISTA
        STOP
C
  903   PRINT * , 'ERROR - EOF on input unit.'
        STOP
C
  904   PRINT * , 'ERROR - NGWRIN returns bad status:',ISTA
        STOP
C
  905   PRINT * , 'ERROR - NGWRFL returns bad status:',ISTA
        STOP
C
      END
