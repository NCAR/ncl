C
C	$Id: rdline.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE RDLINE(IOS, STATUS)
C
C  Return the next non-comment line in the file.
C
C
C  OUTPUT
C       IOS     --  I/O status flag.  This flag is valid only
C                   if STATUS indicates an error.
C       STATUS  --  The error status as defined in COMMON CAPERR.
C
C
      COMMON /CAPERR/ ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1                FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR,
     2                UNDASC, DEVERR, DOCERR, TBLERR , STSZER, ENTERR,
     3                TABERR, TABSER, PRSFIL
      INTEGER ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1        FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR, UNDASC,
     2        DEVERR, DOCERR, STSZER, ENTERR, TABERR, TABSER, TBLERR,
     3        PRSFIL
      COMMON /CAPIOB/ UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPIO2/ LINE, GRNM
      INTEGER LNMAX
      PARAMETER (LNMAX=80)
      CHARACTER*80 LINE
      CHARACTER*80 GRNM
      INTEGER UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /PARTB1/ PART1, KEYSEP, KEYTER, FRMCOM
      COMMON /PARTB2/ PART2, PART3, PART4, PART5, CTSTR, CTLOC
C
C  THE NUMBER OF WORDS IN THE SEARCH PATH MUST BE BIG ENOUGH TO HOLD
C       THE NUMBER OF BITS PER PATH TIMES THE NUMBER OF LEVELS
C
      INTEGER WHSIZE
      PARAMETER (WHSIZE=20)
C
      INTEGER PARTSZ, OTHSZ, NTABLE
      PARAMETER(PARTSZ=3000, OTHSZ=150, NTABLE=50)
      CHARACTER*1 KEYSEP,KEYTER
      CHARACTER*1 FRMCOM(2)
      INTEGER PART2(OTHSZ), PART3(OTHSZ), PART4(NTABLE), PART5(NTABLE*2)
      CHARACTER*1 PART1(PARTSZ)
      INTEGER CTSTR, CTLOC
C
C  CONTROL THE PRINTING OF INPUT LINES
C
      COMMON /CAPSKP/ SKIPIT
      LOGICAL SKIPIT
C
      INTEGER IOS, STATUS
      INTEGER II
      CHARACTER*10 CTMP
C
C  Read in the next buffer from the file.
C
 10   CONTINUE
      CALL CHRRED(UNIT, LNMAX, LINE, IPTR, IOS, STATUS)
C
C  If error, then return.
C
      IF (STATUS.NE. ALLOK) RETURN
C
C  Set the buffer size.
C
      LSIZE = IOS
      IOS = 0
C
C  Echo the line if requested.
C
      IF (.NOT.SKIPIT .AND. LSIZE .NE. 0)
     -        WRITE(6,1)LINE(1:LSIZE)
C
C  Check if this line is big enough to be a comment line.
C
      IF (LSIZE .LT. 2) RETURN
C
C  Check if this line is a special NCAR separator line.
C
      DO 60 I=1,10
      II = IPTR+I-1
      CTMP(I:I) = LINE(II:II)
   60 CONTINUE
      IF (CTMP .EQ. 'NCAR_FILE_') THEN
        WRITE(6,500) LINE(1:LSIZE)
        STOP
      ENDIF
C
C  Check if this line is a comment line.
C
      II = IPTR+1
      IF (FRMCOM(1) .EQ. LINE(IPTR:IPTR) .AND.
     1    FRMCOM(2) .EQ. LINE(II:II) ) THEN
C
C  If this is the first line in the file, attempt to get the
C  graphcap name and set NFLG to flag special graphcaps that
C  require special treatment.
C
C  The special graphcaps and flags are:
C
C          NFLG  graphcap
C          ----  --------
C            1   qms
C
        IF (NFST .EQ. 1) THEN
          NFST = 0
          GRNM = ' '
          DO 20 I=IPTR+2,LSIZE
            IF (LINE(I:I) .EQ. ' ') GO TO 20
            J = I
            K = 1
            DO 30 L=J,LSIZE
              IF (LINE(L:L) .NE. ' ') THEN
                GRNM(K:K) = LINE(L:L)
                K = K+1
              ELSE
                GO TO 40
              ENDIF
   30       CONTINUE
   40       CONTINUE
            IF (GRNM(1:6) .EQ. 'qms800') THEN
              NFLG = 1
            ENDIF
            GO TO 10
   20     CONTINUE
        ENDIF
        GO TO 10
      ENDIF
C
C  Not a comment line, so return.
C
      RETURN
 1    FORMAT(A80)
  500 FORMAT(' READLN -- An NCAR separator line appears in the input, pl
     -ease remove, LINE='/(A80))
      END
