C
C	$Id: sfskbk.f,v 1.2 2000-07-11 21:29:50 haley Exp $
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
C

      SUBROUTINE SFSKBK(IOS, STATUS)
C
C  Set COMMON variable IPTR to point to the next non-blank
C  character in the file, the current line of which is stored
C  in character array LINE.
C
C  OUTPUT
C       IOS    - The I/O status, this value has meaning only if an 
C                error condition obtains (i.e. only if STATUS .GT. 0).
C       STATUS - Status indicator, see subroutine SFPRCF for details.
C
      include 'fnttab.h'
      include 'fnterr.h'
      include 'fntcom.h'
C
      INTEGER IOS, STATUS
      CHARACTER*1 BLANK
      INTEGER II
C
      DATA BLANK/' '/
C
C  Scan the current line for a non-blank.
C
 10   CONTINUE
      DO 20 II = IPTR,LSIZE
        IF (LINE(II:II) .NE. BLANK) THEN
C
C  Non-blank found.
C
          IPTR = II
          RETURN
        END IF
 20   CONTINUE
C
C  End of current line and no no non-blank has been found,
C  get another line.
C
      CALL CFRDLN (ID1,ID2,STATUS)
C
C  Return if EOF or bad read.
C
      IF (STATUS.NE.ALLOK) RETURN
C
C  Go back and scan this line.
C
      GO TO 10
C
      END
