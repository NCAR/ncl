C
C $Id: WriteNameFile.f,v 1.5 2008-07-27 01:04:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM WRNMFI
C
C This program reads an ASCII file defining an EZMAP dataset and writes
C the name information from it to a binary file.
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
C Dimension the arrays into which the data are to be read.
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
C Declare the variable into which names are copied and from which they
C are written.
C
        CHARACTER*64 NAME
C
C Declare an array to use as an output buffer.
C
        CHARACTER*1 CHRS(1024)
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
C Open the output name data file.
C
        FLNM(LFNM+1:LFNM+6)='.names'
        FLNM(LFNM+7:LFNM+7)=CHAR(0)
        CALL NGOFWO (FLNM(1:LFNM+7),IFDE,ISTA)
        IF (ISTA.NE.0) GO TO 904
C
C Read past the next input record.
C
  101   READ (1,'(4I4,4F10.5)',ERR=905,END=906) (INTS(I),I=1,4),
     +                                          (FLTS(I),I=1,4)
C
C If lat/lon data follow, read past them as well.
C
        IF (LOSR.NE.0) THEN
          READ (1,'(8F10.5)',ERR=905,END=906) (FLTS(4+I),I=1,LOSR)
          GO TO 101
        END IF
C
C Zero the count of characters sent to the output file so far.
C
        NCHR=0
C
C Read a line containing an area identifier, an area type, a suggested
C color index, a parent area identifier, and a name.
C
  102   READ (1,'(I4,2I2,I5,1X,A64)',ERR=905,END=906)
     +                                          IAID,IATY,ISCI,IPAR,NAME
C
C Unless the final line has been read, copy the information to the
C output name data file.
C
        IF (IAID.NE.0) THEN
          CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
          CALL WRNUMB (IFDE,CHRS,1024,NCHR,IAID)
          CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
          CALL WRNUMB (IFDE,CHRS,1024,NCHR,IATY)
          CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
          CALL WRNUMB (IFDE,CHRS,1024,NCHR,ISCI)
          CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
          CALL WRNUMB (IFDE,CHRS,1024,NCHR,IPAR)
          CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
          IBEG=MPIFNB(NAME)
          IEND=MPILNB(NAME)
          CALL WRNUMB (IFDE,CHRS,1024,NCHR,IEND-IBEG+1)
          CALL WRCHAR (IFDE,CHRS,1024,NCHR,        ' ')
          DO 103 I=IBEG,IEND
            CALL WRCHAR (IFDE,CHRS,1024,NCHR,NAME(I:I))
  103     CONTINUE
          GO TO 102
        END IF
C
C If there's something left in the output buffer, dump it.
C
        IF (NCHR.GT.0) THEN
          CALL NGWRCH (IFDE,CHRS,NCHR,ISTA)
          IF (ISTA.LE.0) GO TO 907
        END IF
C
C Close the output name data file.
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
  907   PRINT * , 'ERROR - NGWRCH returns bad status:',ISTA
        STOP
C
      END



      SUBROUTINE WRNUMB (IFDE,CHRS,MCHR,NCHR,INUM)
C
        CHARACTER*1 CHRS(MCHR)
C
C Translate the integer INUM to a character form and put it in the
C buffer CHRS.
C
        CHARACTER*1 CTMP(20)
C
C Copy the integer to a local variable.
C
        ITMP=INUM
C
C Generate digits from right to left in the temporary buffer CTMP.
C
        NDGT=0
C
  101   NDGT=NDGT+1
C
        IF (NDGT.GT.20) THEN
          PRINT * , 'WRNUMB - INTEGER TOO BIG - STOP'
          STOP
        END IF
C
        CTMP(NDGT)=CHAR(ICHAR('0')+MOD(ITMP,10))
C
        ITMP=ITMP/10
        IF (ITMP.NE.0) GO TO 101
C
C Send the digits to the output buffer in correct order.
C
        DO 102 IDGT=NDGT,1,-1
          CALL WRCHAR (IFDE,CHRS,MCHR,NCHR,CTMP(IDGT))
  102   CONTINUE
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE WRCHAR (IFDE,CHRS,MCHR,NCHR,ICHR)
C
        CHARACTER*1 CHRS(MCHR),ICHR
C
C Put the character ICHR in the buffer CHRS.
C
        NCHR=NCHR+1
        CHRS(NCHR)=ICHR
C
C If the buffer is now full, dump it to the file specified by IFDE.
C
        IF (NCHR.GE.MCHR) THEN
          CALL NGWRCH (IFDE,CHRS,NCHR,ISTA)
          IF (ISTA.LE.0) THEN
            PRINT * , 'WRCHAR - BAD STATUS FROM NGWRCH = ',ISTA
            STOP
          END IF
          NCHR=0
        END IF
C
C Done.
C
        RETURN
C
      END



      INTEGER FUNCTION MPIFNB (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of MPIFNB(CHRS) is the index of the first non-blank in the
C character string CHRS.
C
        DO 101 I=1,LEN(CHRS)
          IF (CHRS(I:I).NE.' ') THEN
            MPIFNB=I
            RETURN
          END IF
  101   CONTINUE
C
        MPIFNB=1
C
        RETURN
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
