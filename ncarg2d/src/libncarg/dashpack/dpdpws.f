C
C $Id: dpdpws.f,v 1.3 2008-07-27 00:16:58 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPDPWS (DPCH,DSYM,DPAT)
C
        CHARACTER*(*) DPCH,DSYM,DPAT
C
C This routine is called with three character strings of the same
C length: the input string DPCH is a basic dash pattern, to be used
C with DASHPACK, containing some characters that are to be treated
C as symbol generators; the input string DSYM contains characters
C other than blanks or minus signs in the positions corresponding to
C characters of DPCH that are to be treated as symbol generators; and
C the output string DPAT is the appropriate character string to be
C input to DASHPACK by a call to DPSETC.
C
C For example, to get a dash pattern specifying alternate solid-line
C segments and filled circles, use this:
C
C   DPCH: '$0'  !  (The '$' means "solid"; the '0' selects a symbol.)
C   DSYM: ' #'  !  (Use any character but ' ' or '-' in lieu of '#'.)
C
C To get a dash pattern specifying a solid section, the characters
C "I=1", another solid section, and a hollow-circle symbol, use this:
C
C   DPCH: '$$$I|=|1$$$5'  !  (A final '5' selects the desired symbol.)
C   DSYM: '-----------+'  !  (A plus sign marks the symbol position.)
C
C (Note the use of break characters - '|' - in the character string to
C allow the string to be written bending with a curve being drawn.)
C
C By default, symbol-selection characters which may be used include
C the following:
C
C   0 => filled circle         5 => hollow circle
C   1 => filled square         6 => hollow square
C   2 => filled triangle       7 => hollow triangle
C   3 => filled diamond        8 => hollow diamond
C   4 => filled star           9 => hollow star
C
C See also the DASHPACK routine DPDSYM; it actually draws the symbols
C and may be replaced by a user to draw a different set.
C
        LPAT=LEN(DPAT)
C
        DO 101 I=1,LPAT
          IF (DSYM(I:I).EQ.' '.OR.DSYM(I:I).EQ.'-') THEN
            DPAT(I:I)=DPCH(I:I)
          ELSE
            DPAT(I:I)=CHAR(IOR(ICHAR(DPCH(I:I)),128))
          END IF
  101   CONTINUE
C
        RETURN
C
      END
