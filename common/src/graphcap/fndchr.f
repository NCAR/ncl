C
C	$Id: fndchr.f,v 1.2 2000-07-11 21:31:57 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this library; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C

      LOGICAL FUNCTION FNDCHR (CHAR)
C
C  Test the current position in the input buffer for a match with
C  CHAR.  If a match is found, FNDCHR is set to .TRUE. and the
C  buffer pointer is bumped by one; if no match is found, FNDCHR
C  is set to .FALSE. and the buffer pointer is unchanged.  No
C  reloading of the input buffer is done. As a special condition,
C  a match is found if CHAR is blank and the buffer is at the
C  end-of-line.
C
C  OUTPUT
C       FNDCHR  --  Logical value indicating whether a match was found.
C
      COMMON /CAPIOB/ UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPIO2/ LINE, GRNM
      INTEGER LNMAX
      PARAMETER (LNMAX=80)
      CHARACTER*80 LINE
      CHARACTER*80 GRNM
      INTEGER UNIT, IPTR, LSIZE, NFST, NFLG
C
      CHARACTER*1 CHAR
      CHARACTER*1 BLANK
      DATA BLANK/' '/
C
C  If at END-OF-LINE and CHAR is blank, then set match.
C
      IF (IPTR.GT.LSIZE .AND. CHAR.EQ.BLANK) THEN
        FNDCHR = .TRUE.
        IPTR = IPTR + 1
        RETURN
      END IF
C
C  Check for a match.
C
      IF (LINE(IPTR:IPTR) .EQ. CHAR) THEN
        FNDCHR = .TRUE.
        IPTR = IPTR + 1
        RETURN
      END IF
C
C  No match.
C
      FNDCHR = .FALSE.
      RETURN
      END
