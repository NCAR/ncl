C
C	$Id: cfrdln.f,v 1.4 2008-07-27 12:23:41 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE CFRDLN(CNUM,CWID,IER)
C
C  Read a line from the input fontcap.  Keep reading until
C  a non-blank, or non-comment, line is encountered.  If the
C  line is a BEGIN_CHAR line, return the character number and
C  character width.
C
      include 'fntcom.h'
      include 'fnttab.h'
C
      PARAMETER(LLEN=81)
C
      INTEGER      CNUM,CWID,IER
      INTEGER      IOS ,STATUS  ,TLENS(10)
      CHARACTER*81  LTMP
      CHARACTER*80 TOKENS(15),TLINE
C
   30 CONTINUE
      IER  = 0
      CNUM = 0
      CWID = 0
      CALL CHRRED(UNIT, LLEN, LTMP, IPTR, IOS, STATUS)
C
      IF (STATUS .EQ. 1) THEN
        IER = EEOF
        RETURN
      ELSE IF (STATUS .GT. 1) THEN
        IER = STATUS
        RETURN
      ELSE 
        LINE = ' '
        LSIZE = IOS
        DO 10 I=1,IOS
          LINE(I:I) = LTMP(I:I)
   10   CONTINUE
        IF (VERBOS .NE. 0) WRITE(6,510) LINE
        IF (LINE .EQ. BLNKL) GO TO 30
        IF (LINE(1:10) .EQ. 'BEGIN_CHAR') THEN
          CALL CHRRED(UNIT, LLEN, LTMP, IPTR, IOS, STATUS)
          IF (VERBOS .NE. 0) THEN
            TLINE = ' '
            DO 40 I=1,IOS
              TLINE(I:I) = LTMP(I:I)
   40       CONTINUE
            WRITE(6,510) TLINE
          ENDIF
          CALL CHRRED(UNIT, LLEN, LTMP, IPTR, IOS, STATUS)
          TLINE = ' '
          DO 20 I=1,IOS
            TLINE(I:I) = LTMP(I:I)
   20     CONTINUE
          IF (VERBOS .NE. 0) WRITE(6,510) TLINE
          CALL FFGTTK(TLINE,NUMT,TOKENS,TLENS)
          READ(TOKENS(3),500) CNUM
          READ(TOKENS(4),500) CWID
  500     FORMAT(I6)
  510     FORMAT(A80)
          RETURN
        ENDIF
        IF (LINE(1:2).EQ.'/*') GO TO 30
        RETURN
      ENDIF
C
      END
