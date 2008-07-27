C
C	$Id: findg.f,v 1.5 2008-07-27 00:59:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM FINDG
C
C  PURPOSE             This program reads an input FORTRAN file
C                      from the standard input and prints out
C                      those lines (along with line numbers)
C                      which contain calls to old plotting
C                      entries from the pre-GKS NCAR plot package.
C                      In conversions to GKS, the user should
C                      examine these calls for possible changes.
C                      Many entries will require no change.
C                      For details on needed changes, see the
C                      UserDoc "Converting from pre-GKS NCAR
C                      Graphics to NCAR Graphics Version 3.00."
C
C                      There are two sets of file names which can
C                      be used for searching.  The default set is
C                      the set which is described in the UserDoc
C                      mentioned above.  This is a core set of entries
C                      that will usually catch most any call that
C                      might possibly need conversion.  If a "$ALL"
C                      is encountered in columns 1 through 4 of any
C                      input card, then the defualt set of search
C                      names is replaced with a complete set.  This
C                      complete set constitutes all possible names
C                      which may need conversion.  Detailed descriptions
C                      for conversion procedures do not exist for all
C                      of the entries in the complete set.
C
C                      If the single character $ is read in column 1
C                      of any input line, then a complete
C                      list of the entry points being searched for
C                      is printed, and execution is terminated.
C
C  USAGE               Compile and execute the program using a FORTRAN
C                      code as input from the standard input.
C
C  ENTRY POINTS        FINDG
C
C  I/O                 Reads from standard input, writes to standard
C                      output.
C
C  LANGUAGE            FORTRAN 77
C
C  HISTORY             Originally written in March, 1984.
C
C  NOTE                All names are converted to uppercase.
C
C
      PARAMETER ( NUM=71 , NUMA=468 )
      COMMON /SUBNMS/SUBNAM,ALLNAM
      CHARACTER*10 SUBNAM(NUM),ALLNAM(NUMA)
      CHARACTER*10 SRCNMS(NUMA)
      CHARACTER*80  STRING
      CHARACTER*66  LINE
      LOGICAL CALLS, NOTFND
      INTEGER COLPOS, CHARS
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL FNGBKD
C
C  Initialize variables.
C
      LCOUNT = 0
      CALLS = .FALSE.
      NOTFND = .TRUE.
      DO 50 I=1,NUMA
        SRCNMS(I) = SUBNAM(I)
   50 CONTINUE
      NUMF = NUM
C
C  Main loop for reading lines.
C
 100  LCOUNT = LCOUNT + 1
      DO 110 I = 1,80
            STRING(I:I) = ' '
 110  CONTINUE
      READ(5,1001,ERR=903,END=902) STRING
C
C  Check for '$' in column 1.  If there is a $ in column 1,
C  then check for a "$ALL".  If $ALL, then reset search names;
C  otherwise print search names and stop.
C
      IF (STRING(1:1).EQ.'$') THEN
        IF (STRING(2:2).EQ.'A' .OR. STRING(2:2).EQ.'a') THEN
          WRITE(6,1002)
          NUMF = NUMA
          DO 120 I = 1,NUMF
            SRCNMS(I) = ALLNAM(I)
  120     CONTINUE
        ELSE
          GO TO 905
        ENDIF
      ENDIF
C
C  Check for comment line.
C
      IF (STRING(1:1).EQ.'C' .OR. STRING(1:1).EQ.'c') GOTO 100
      DO 200 I = 1,72
 200  IF (STRING(I:I) .NE. ' ') GOTO 210
      GOTO 100
 210  CONTINUE
C
C  Compress line by removing all blank characters.
C
      DO 240 I = 1,66
 240  LINE(I:I) = ' '
      NEXTCH = 0
      DO 250 I = 1,72
      IF (STRING(I:I) .NE. ' ') THEN
        NEXTCH = NEXTCH + 1
        LINE(NEXTCH:NEXTCH) = STRING(I:I)
      ENDIF
 250  CONTINUE
      LASTCH = NEXTCH
C
C  Is this a continuation line for a statement having a subroutine
C  call in it?
C
      IF (CALLS) THEN
        COLPOS = 2
        GOTO 410
      ENDIF
C
C  Are there any subroutine calls on this line?
C
      DO 300 J = 1,66
      CALL LW2UP(LINE(J:J+3))
      IF (LINE(J:J+3).EQ.'CALL') GOTO 400
 300  CONTINUE
C
C  No calls, read another line.
C
      GOTO 100
C
C  Current line has a CALL.
C
 400  COLPOS = J + 4
 410  IF (COLPOS .GE. LASTCH) THEN
        CALLS = .TRUE.
        GOTO 100
      ENDIF
C
C  Find the last column position of the name of the
C  subroutine being called.
C
      DO 490 I = COLPOS,LASTCH
      IF (LINE(I:I) .EQ. '(') THEN
        LASTSU = I - 1
        GOTO 495
      ENDIF
 490  CONTINUE
      LASTSU = LASTCH
 495  CHARS = LASTSU + 1 - COLPOS
C
C  Is the called subroutine a graphics subroutine?
C
      DO 500 I = 1,NUMF
C
C  Convert to upper case.
C
      CALL LW2UP(LINE(COLPOS:LASTSU))
C
C  Check for a match.
C
      IF ( LINE(COLPOS:LASTSU).EQ.SRCNMS(I)(1:CHARS)) THEN
         NOTFND = .FALSE.
         WRITE(6,1003) LCOUNT,STRING
         GOTO 600
      ENDIF
 500  CONTINUE
 600  CALLS = .FALSE.
      GOTO 100
C
 902  WRITE(6,'('' END OF FILE '')')
      IF (NOTFND) THEN
        WRITE(6,'('' NO GRAPHICS SUBROUTINES FOUND '')')
      ENDIF
      ICOUNT = LCOUNT - 1
      WRITE(6,1004)ICOUNT
      STOP
C
 903  WRITE(6,'('' ***ERROR IN READ*** '')')
      STOP
C
 905  WRITE(6,1005) (SRCNMS(I),I=1,NUMF)
      STOP
C
1001  FORMAT(A80)
1002  FORMAT(' FINDG -- FULL SET OF SEARCH NAMES SELECTED'/)
1003  FORMAT(' LINE ',I4,':',A66/)
1004  FORMAT(' TOTAL NUMBER LINES READ : ',I4)
1005  FORMAT(' SEARCHING FOR :'//,(6A10))
C
      END
