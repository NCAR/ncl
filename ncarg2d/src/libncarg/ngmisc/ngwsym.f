C
C $Id: ngwsym.f,v 1.6 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGWSYM(FTYPE,NUM,X,Y,SIZE,ICOLOR,IALT)
C
C  Draw a glyph from the meteorological fonts.
C
C ARGUMENTS
C
C ON INPUT  
C    
C  FTYPE   The meteorological font specification.  FTYPE is a type
C          character variable.  Legal values are:
C
C            'WW' -- Present weather.
C            'C'  -- Cloud types.
C            'CL' -- Low clouds.
C            'CM' -- Medium clouds.
C            'CH' -- High clouds.
C            'W'  -- Past weather.
C            'N'  -- Sky cover.
C            'a'  -- Pressure tendency.
C
C  NUM     The number of the glyph within the given font.  For example,
C          the glyph for haze is number five in the WW font.
C
C  X       The X coordinate position in world coordinates where the 
C          glyph is to be positioned.  This X position marks the 
C          horizontal center of the glyph.
C
C  Y       The Y coordinate position in world coordinates where the 
C          glyph is to be positioned.  This Y position marks the 
C          vertical center of the glyph.
C
C  SIZE    The size of the character in world coordinate Y units.
C
C  ICOLOR  The color index indicating the color that the glyph is to
C          be drawn with.  If you want the default foreground color,
C          set this parameter to "1".
C
C  IALT    If IALT=1, then the alternate glyph for the one specified
C          is drawn.  This applies only to a few glyphs such as
C          numbers 7, 93, 94, 95, 97 in the WW font and number 3 
C          in the W font.
C
C
C
C EXAMPLES
C
C    CALL NGWSYM('N',6,.5,.5,.25,1,0)
C  
C      would plot a circle that is 3/4th filled at position (.5,.5)
C      and height .25 in the foreground color.
C
C    CALL NGWSYM('a',6,.2,.8,.3,1,0)
C
C      would plot the glyph for barometric pressure that is falling 
C      then steady.
C
C    CALL NGWSYM('WW',95,.5,.5,.2,1,1)
C
C      would plot the alternate glyph for slight or moderate 
C      thunderstorm without hail.
C
      CHARACTER*(*) FTYPE
      CHARACTER*2   FTMP
      CHARACTER*1   CCHR
      INTEGER       NUM,ICOLOR,IALT
      REAL          X,Y,SIZE
C
      IF (LEN(FTYPE) .GE. 2) THEN
        FTMP(1:2) = FTYPE(1:2)
      ELSE
        FTMP(1:1) = FTYPE(1:1)
        FTMP(2:2) = ' '
      ENDIF
C
      IF (FTMP.EQ.'WW' .AND. (NUM.GE.0 .AND. NUM.LE.99)) THEN
        IF (NUM.GE.0 .AND. NUM.LE.93) THEN
          MFONT = -36
          IF (IALT .NE. 1) THEN
            CCHR  = CHAR(NUM+33)
          ELSE
            IF (NUM .EQ. 07) THEN
              MFONT = -37 
              CCHR = CHAR(39)
            ELSE IF (NUM .EQ. 93) THEN
              MFONT = -37 
              CCHR = CHAR(40)
            ENDIF
          ENDIF
        ELSE IF (NUM.GE.94 .AND. NUM.LE.99) THEN
          MFONT = -37
          IF (IALT .NE. 1) THEN
            CCHR  = CHAR(NUM-94+33)
          ELSE
            IF (NUM .EQ. 94) THEN
              CCHR = CHAR(41)
            ELSE IF (NUM .EQ. 95) THEN
              CCHR = CHAR(42)
            ELSE IF (NUM .EQ. 97) THEN
              CCHR = CHAR(43)
            ENDIF
          ENDIF 
        ENDIF
      ELSE IF (FTMP.EQ.'C ' .AND. (NUM.GE.0 .AND. NUM.LE.9))THEN
        MFONT = -37
        CCHR  = CHAR(NUM+45)
      ELSE IF (FTMP.EQ.'CL' .AND. (NUM.GE.0 .AND. NUM.LE.9))THEN
        MFONT = -37
        IF (NUM .EQ. 0) THEN
          CCHR = ' '
        ELSE
          CCHR  = CHAR(NUM+54)
        ENDIF
      ELSE IF (FTMP.EQ.'CM' .AND. (NUM.GE.0 .AND. NUM.LE.9))THEN
        MFONT = -37
        IF (NUM .EQ. 0) THEN
          CCHR = ' '
        ELSE
          CCHR  = CHAR(NUM+63)
        ENDIF
      ELSE IF (FTMP.EQ.'CH' .AND. (NUM.GE.0 .AND. NUM.LE.9))THEN
        MFONT = -37
        IF (NUM .EQ. 0) THEN
          CCHR = ' '
        ELSE
          CCHR  = CHAR(NUM+72)
        ENDIF
      ELSE IF (FTMP .EQ. 'W ' .AND. (NUM.GE.0 .AND. NUM.LE.9))THEN
        MFONT = -36
        IF (NUM.GE.0 .AND. NUM.LE.2) THEN
          CCHR = ' '
        ELSE IF (NUM .EQ. 3) THEN
          IF (IALT .NE. 1) THEN
            CCHR = CHAR(64)
          ELSE
            CCHR = CHAR(71)
          ENDIF
        ELSE IF (NUM .EQ. 4) THEN
          CCHR = CHAR(78)
        ELSE IF (NUM .EQ. 5) THEN
          CCHR = CHAR(83)
        ELSE IF (NUM .EQ. 6) THEN
          CCHR = CHAR(93)
        ELSE IF (NUM .EQ. 7) THEN
          CCHR = CHAR(103)
        ELSE IF (NUM .EQ. 8) THEN
          MFONT = -37
          CCHR = CHAR(44)
        ELSE IF (NUM .EQ. 9) THEN
          CCHR = CHAR(50)
        ENDIF
      ELSE IF (FTMP.EQ.'N ' .AND. (NUM.GE.0 .AND. NUM.LE.9))THEN
        MFONT = -37
        CCHR = CHAR(NUM+82)
      ELSE IF (FTMP.EQ.'a ' .AND. (NUM.GE.0 .AND. NUM.LE.9))THEN
        MFONT = -37
        IF (NUM .EQ. 9) THEN
          CCHR = ' '
        ELSE
          CCHR = CHAR(NUM+92)
        ENDIF
      ELSE
        WRITE(6,100) FTMP,NUM
  100   FORMAT(
     +     ' NGWSYM -- Error in font or glyph number specification:',
     +     / '             Font = ',A2,
     +     / '             Glyph number =',I3)
        GO TO 10
      ENDIF
C
      CALL NGPUTG(MFONT,CCHR,X,Y,SIZE,ICOLOR)
C
   10 CONTINUE
C
      RETURN
      END
