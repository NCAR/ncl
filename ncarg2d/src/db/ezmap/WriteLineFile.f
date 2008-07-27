C
C $Id: WriteLineFile.f,v 1.5 2008-07-27 01:04:30 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM WRLNFI
C
C This program reads an ASCII file defining an EZMAP dataset and writes
C the line data from it to a binary file.
C
C The line data in the input files are grouped into logical records,
C each of which represents a single line segment and exists as a pair
C of FORTRAN records.  The first record of each pair contains four
C integers and four reals; if the first integer has a zero value, then
C there is no second record and the end of the line data has been
C reached.  The second record of each pair contains a varying number of
C real lat/lon coordinate pairs.  In terms of the variables into which
C the data are read:
C
C   INTS(1) = the number of reals in the second FORTRAN record, but if
C             the value is 0, there is no second FORTRAN record: this
C             signals the end of the line data in the input file.
C   INTS(2) = a "line type" for the line segment.
C   INTS(3) = the left area identifier for the line segment.
C   INTS(4) = the right area identifier for the line segment.
C   FLTS(1) = the biggest latitude in the line segment.
C   FLTS(2) = the smallest latitude in the line segment.
C   FLTS(3) = the biggest longitude in the line segment.
C   FLTS(4) = the smallest longitude in the line segment.
C  (FLTS(I),I=5,4+INTS(1)) = lat/lon pairs for the line segment.
C
C Following the line data are the names of the global areas described
C by the lines.  Each record contains an area identifier, an area type,
C a suggested color index for the area, the identifier of the "parent"
C area, and the name of the area.
C
C Dimension the arrays into which the data are to be read and from which
C they are to be written.
C
        DIMENSION INTS(4),FLTS(204)
C
C Set up an equivalent name for the array element specifying the length
C of the second FORTRAN record.
C
        EQUIVALENCE (INTS(1),LOSR)
C
C Declare a character variable to hold the dataset name.
C
        CHARACTER*71 FLNM
C
C Get the name of the file to be read.
C
        PRINT * , ' '
        PRINT * , 'Enter the name of the file to be read:'
        READ (*,'(A64)') FLNM
C
        IF (FLNM.EQ.' ') GO TO 901
C
        LFNM=MPILNB(FLNM)
C
C Open the specified input file.
C
        OPEN (1,FILE=FLNM(1:LFNM),STATUS='OLD',FORM='FORMATTED',ERR=902)
C
        REWIND 1
C
C Open the output line data file.
C
        FLNM(LFNM+1:LFNM+6)='.lines'
        FLNM(LFNM+7:LFNM+7)=CHAR(0)
        CALL NGOFWO (FLNM(1:LFNM+7),IFDE,ISTA)
        IF (ISTA.NE.0) GO TO 904
C
C Read the next input record and copy the integer part of it to the
C output file.
C
  101   READ (1,'(4I4,4F10.5)',ERR=905,END=906) (INTS(I),I=1,4),
     +                                          (FLTS(I),I=1,4)
C
        CALL NGWRIN (IFDE,INTS,4,ISTA)
        IF (ISTA.NE.4) GO TO 907
C
C If lat/lon data follow, copy them to the output file and go back for
C another data record.
C
        IF (LOSR.NE.0) THEN
          READ (1,'(8F10.5)',ERR=905,END=906) (FLTS(4+I),I=1,LOSR)
          CALL NGWRFL (IFDE,FLTS,4+LOSR,ISTA)
          IF (ISTA.NE.4+LOSR) GO TO 908
          GO TO 101
        END IF
C
C Close the output line data file.
C
        CALL NGCLFI (IFDE)
C
C Close the input file.
C
        CLOSE (1,ERR=903)
C
C Normal exit.
C
        STOP
C
C Error exits.
C
  901   PRINT * , 'ERROR - Input file name is blank.'
        STOP
C
  902   PRINT * , 'ERROR - Couldn''t open input file.'
        STOP
C
  903   PRINT * , 'ERROR - Couldn''t close input file.'
        STOP
C
  904   PRINT * , 'ERROR - NGOFWO returns bad status:',ISTA
        STOP
C
  905   PRINT * , 'ERROR - FORTRAN error on READ of input file.'
        STOP
C
  906   PRINT * , 'ERROR - Unexpected EOF on READ of input file.'
        STOP
C
  907   PRINT * , 'ERROR - NGWRIN returns bad status:',ISTA
        STOP
C
  908   PRINT * , 'ERROR - NGWRFL returns bad status:',ISTA
        STOP
C
      END



      INTEGER FUNCTION MPILNB (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of MPILNB(CHRS) is the index of the last non-blank in the
C character string CHRS.
C
        DO 101 I=LEN(CHRS),1,-1
          IF (CHRS(I:I).NE.' ') THEN
            MPILNB=I
            RETURN
          END IF
  101   CONTINUE
C
        MPILNB=1
C
        RETURN
C
      END
