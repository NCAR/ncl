C
C	$Id: sffndc.f,v 1.3 2000-08-22 03:53:13 haley Exp $
C
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C

      LOGICAL FUNCTION SFFNDC (CHAR)
C
C  Test the current position in LINE for the presence of CHAR.
C  If a match, then set SFFNDC to .TRUE. .  If no match, then set
C  SFFNDC to .FALSE. .
C
C  This routine assumes that no reloading of the line is necessary.
C
C  Note that a match is found if CHAR is BLANK and at END-OF-LINE.
C
C  OUTPUT
C	SFFNDC - Logical value indicating the presence of CHAR
C
C  COMMON variable IPTR -  If CHAR is found, then IPTR will point to
C                          the next position in the line;  if CHAR
C                          is not found,  then IPTR is unchanged.
C
      include 'fntcom.h'
      include 'fnttab.h'
C
      CHARACTER*1 CHAR
C
      CHARACTER*1 BLANK
      DATA BLANK/' '/
C
C  If END-OF-LINE and CHAR is BLANK, then set match.
C
      IF (IPTR.GT.LSIZE .AND. CHAR.EQ.BLANK) THEN
	SFFNDC = .TRUE.
        IPTR = IPTR + 1
        RETURN
      END IF
C
C  Check for a match.
C
      IF (LINE(IPTR:IPTR) .EQ. CHAR) THEN
	SFFNDC = .TRUE.
	IPTR = IPTR + 1
	RETURN
      END IF
C
C  Failure, so set SFFNDC to .FALSE. .
C
      SFFNDC = .FALSE.
      RETURN
      END
