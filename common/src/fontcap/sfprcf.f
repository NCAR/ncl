C
C	$Id: sfprcf.f,v 1.4 2008-07-27 12:23:42 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C

      SUBROUTINE SFPRCF (ITYP,IOS, STATUS)
C
C  Process the input fontcap; define the output arrary in
C  COMMON block CAPFNT.
C
C  INPUT parameters:
C    None.
C
C  OUTPUT parameters:
C    ITYP    --  Type of fontcap: 0 for stroked; 1 for filled.
C    IOS     --  I/O status flag which has meaning only if there was
C                an I/O error (i.e. only if STATUS.GT.0).
C    STATUS  --  Integer-valued error status:
C                  =  0 -- No errors
C                  =  1 -- EOF encountered.
C                  =  2 -- Error decoding an integer value.
C                  =  3 -- Integer exceeds max. number of digits allowed.
C                  =  8 -- Error decoding floating-point value.
C                  =  9 -- Floating value exceeds maximum
C                          number of digits allowed.
C                  = 10 -- Not an integer field.
C                  = 11 -- String buffer not big enough
C                          to hold the input string.
C                  = 12 -- Non-ASCII value encountered.
C                  = 13 -- Keyword FONT_TYPE has not been defined.
C
      include 'fnterr.h'
      include 'fnttab.h'
      include 'capfnt.h'
      include 'fntcom.h'
C     
      INTEGER IOS, STATUS
      INTEGER WHICH(WHSIZE), ROW, ROW1, CURPOS, DUMMY
      INTEGER CURTMP, CHCNT
      INTEGER II,XIN,YIN,PEIN,PSIN,PIN
      INTEGER I1MACH
C
C  CURCHR  --  Number of the current character.
C  CURSTK  --  Stroke number (in the sequence of all strokes
C              processed for all characters) of the current
C              character being processed.
C  CBUF    --  Temporary stroke buffer (a single word of the CBUF
C              array is packed for each line encountered in
C              any stroke table.)
C
      INTEGER CURCHR, CURSTK, CBUF(1000)
C
C  Get the number of bits per word.
C
      INTEGER BTSWRD
      BTSWRD = I1MACH(5)
C
C  Initialize.
C
      CURCHR = 1
      CURSTK = 1
      CURPOS = 1
      DO 10 I=1,CHRSM1
      CPNTRS(I) = 0
   10 CONTINUE
      DO 20 I=1,CHRSM2
      CSTRKS(I) = 0
   20 CONTINUE
C
C  Get the first line of the file.
C
      CALL CFRDLN(ID1,ID2,STATUS)
      IF (STATUS .NE. ALLOK) RETURN
C
C  Process a filled font if requested.
C
      ITYP = 0
      IF (LINE(1:9) .EQ. 'FONT_NAME') THEN
        CALL FFPRSA(STATUS)
        ITYP = 1
        RETURN
      ENDIF
C
C  Continue processing, look for the next keyword.
C
 1    CONTINUE
      CALL SFSKBK(IOS, STATUS)
      IF (STATUS .NE. ALLOK) RETURN
C
C  Locate the keyword segment.
C  If no keyword is located on the line, then WHICH will
C  be returned as 0.  If a keyword is located, the the
C  first PTHBTS bits of WHICH will contain an integer
C  indicating which class has been found (1 for CHARACTER,
C  2 for COORD, etc.) and the next PTHBTS of WHICH will
C  contain an integer indicating which subclass has been
C  found (if the class is COORD for example, then the
C  second PTHBTS of WHICH will contain either 1 [for X],
C  or 2 [for Y], etc.)  
C
C
      CALL SFGTWK (WHICH, IOS, STATUS)
      IF (STATUS .NE. ALLOK) RETURN
C
C  If no keyword was matched on the current line, get a new line.
C
      IF (WHICH(1) .EQ. 0) GO TO 1
C
C  Get the major class number, and the subheading number.
C
      CALL GBYTES(WHICH, ROW, 0, PTHBTS, 0, 1)
      CALL GBYTES(WHICH, ROW1, PTHBTS, PTHBTS, 0, 1)
C
C  Branch to proper major class parsing.
C
      GO TO (100,200,300,400,500),ROW
C
C  CHARACTER class processing.
C
 100  CONTINUE
      IF (ROW1.EQ.1) THEN
        CALL SFGTIN(CHSTRT, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.2) THEN
        CALL SFGTIN(CHSEND, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3) THEN
        CALL SFGTIN(CHRWDT, 1, DUMMY, IOS, STATUS)
      END IF
      GO TO 9000
C
C  COORD class processing.
C
 200  CONTINUE
      IF (ROW1.EQ.1) THEN
        CALL SFGTIN(CORXST, 1, DUMMY, IOS, STATUS)
        XIN=(BTSWRD-CORXST)-1
      ELSE IF (ROW1.EQ.3) THEN
        CALL SFGTIN(CORYST, 1, DUMMY, IOS, STATUS)
        YIN=(BTSWRD-CORYST)-1
      ELSE IF (ROW1.EQ.5) THEN
        CALL SFGTIN(CORPST, 1, DUMMY, IOS, STATUS)
        PIN=(BTSWRD-CORPST)-1
      ELSE IF (ROW1.EQ.2) THEN
        CALL SFGTIN(CORXLN, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.4) THEN
        CALL SFGTIN(CORYLN, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.6) THEN
        CALL SFGTIN(CORPLN, 1, DUMMY, IOS, STATUS)
      END IF
      GO TO 9000
C
C  PAINT class processing.
C
 300  CONTINUE
      IF (ROW1.EQ.1) THEN
        CALL SFGTIN(PBEGST, 1, DUMMY, IOS, STATUS)
        PSIN=(BTSWRD-PBEGST)-1
      ELSE IF (ROW1.EQ.3) THEN
        CALL SFGTIN(PENDST, 1, DUMMY, IOS, STATUS)
        PEIN=(BTSWRD-PENDST)-1
      ELSE IF (ROW1.EQ.2) THEN
        CALL SFGTIN(PBEGLN, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.4) THEN
        CALL SFGTIN(PENDLN, 1, DUMMY, IOS, STATUS)
      END IF
      GO TO 9000
C
C  CHAR class processing.
C
 400  CONTINUE
      IF (FNTTYP .LT. 0) THEN
        STATUS = 13
        RETURN
      ENDIF
C
      CPNTRS(CURCHR) = CURSTK
      CURCHR = CURCHR + 1
      IF (FNTTYP.EQ.1 .OR. FNTTYP.EQ.3) THEN
C  Process character left and character right values.
        CALL SFGTIN(CBUF,2,CHCNT,IOS,STATUS)
        CURTMP = 0
        CALL SBYTES(CURTMP,CBUF(1),XIN,CORXLN,0,1)     
        CALL SBYTES(CURTMP,CBUF(2),YIN,CORYLN,0,1)   
        CSTRKS(CURSTK) = CURTMP
        CPNLST = CURSTK
        CURSTK = CURSTK+1
      ENDIF
C  Process stroke data (no paint start/end data for filled fonts).
      IF (FNTTYP.EQ.2 .OR. FNTTYP.EQ.3) THEN
        ISMX = 600
      ELSE IF (FNTTYP.EQ.0 .OR. FNTTYP.EQ.1) THEN
        ISMX = 1000
      ENDIF
      CALL SFGTIN(CBUF, ISMX, CHCNT, IOS, STATUS)
      IF (FNTTYP.EQ.2 .OR. FNTTYP.EQ.3) THEN
        ISINC = 3
      ELSE IF (FNTTYP.EQ.0 .OR. FNTTYP.EQ.1) THEN
        ISINC = 5
      ENDIF
      DO 410 II = 1,CHCNT,ISINC
      CURTMP = 0
      CALL SBYTES(CURTMP,CBUF(II),XIN,CORXLN,0,1)     
      CALL SBYTES(CURTMP,CBUF(II+1),YIN,CORYLN,0,1)   
      CALL SBYTES(CURTMP,CBUF(II+2),PIN,CORPLN,0,1)   
      IF (FNTTYP.EQ.0 .OR. FNTTYP.EQ.1) THEN
        CALL SBYTES(CURTMP,CBUF(II+3),PSIN,PBEGLN,0,1)  
        CALL SBYTES(CURTMP,CBUF(II+4),PEIN,PENDLN,0,1)  
      ENDIF
      CSTRKS(CURSTK) = CURTMP
      CPNLST = CURSTK
      CURSTK = CURSTK + 1
 410  CONTINUE
      GO TO 9000
C
C  FONT class processing.
C
 500  CONTINUE
      IF (ROW1.EQ.1) THEN
        CALL SFGTIN(FNTRHT, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.2) THEN
        CALL SFGTIN(FNTTOP, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3) THEN
        CALL SFGTIN(FNTCAP, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.4) THEN
        CALL SFGTIN(FNTHLF, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.5) THEN
        CALL SFGTIN(FNTBAS, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.6) THEN
        CALL SFGTIN(FNTBOT, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.7) THEN
        CALL SFGTIN(FNTTYP, 1, DUMMY, IOS, STATUS)
      END IF
        GO TO 9000
 9000 CONTINUE
      IF (STATUS .NE. ALLOK) RETURN
C
C  Go back and start the scan again.
C
      GO TO 1
C
      END
