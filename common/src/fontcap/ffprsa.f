C
C	$Id: ffprsa.f,v 1.1 1997-01-24 21:59:52 haley Exp $
C
      SUBROUTINE FFPRSA(IER)
C
C  Parse the ASCII fontcap.
C
      include 'fntcom.h'
C
      DATA IFRST/0/
      PARAMETER (NLEN=40)
      INTEGER ITMP(NLEN)
C
C  Retrieve the font name.
C
      IF (IFRST .EQ. 0) THEN
        IF (LINE(1:9) .NE. 'FONT_NAME') THEN
          IER = EORD
          IFRST = 1
          RETURN
        ELSE
          CALL CFRDLN(IDUM1,IDUM2,IER)
          DO 20 I=1,NUMFTS
            IF (LINE(1:NLEN) .EQ. FNTNMS(I)) THEN
              FNAME = FNTNMS(I)
              IFRST = 1
              GO TO 30
            ENDIF
   20     CONTINUE
          IER = EINM
          RETURN
        ENDIF
      ENDIF
   30 CONTINUE
C
C  Retrieve all keyword values except for the character outlines.
C
      INDX = 1
      DO 40 I=2,NUMKYS
        CALL CFRDLN(IDUM1,IDUM2,IER)
        IF (LINE(1:20) .NE. KEYLST(I)) THEN
          IER = EORD
          RETURN
        ELSE
          CALL CFRDLN(IDUM1,IDUM2,IER)
          CALL FFTKIN(LINE,NUM,ITMP,IER)
          DO 60 J=1,NUM
            INDX = INDX+1
            INTARR(INDX) = ITMP(J)
   60     CONTINUE
        ENDIF 
   40 CONTINUE
C
C  Fill in all of the values for the technical information part
C  of the fontcap and write them to the buffer.
C
      CALL FFINFO(IER)
      IF (IER .NE. 0) RETURN
C
C  Type flag.
C
      CALL SBYTES(BUFFER,TYPFLG,0,16,0,1)
C
C  Font name.
C
      DO 70 I=1,NLEN
        ITMP(I) = ICHAR(FNAME(I:I))
   70 CONTINUE
      CALL SBYTES(BUFFER,ITMP,16,8,0,NLEN)
C
C  Remaining information fields.
C
      CALL SBYTES(BUFFER,INTARR(2),336,16,0,NUMNTR-1)
C
C  Process the characters.
C
      CALL FFPRCF(IER)
C
      RETURN
      END
