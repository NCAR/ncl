C
C	$Id: cfwrit.f,v 1.2 2000-07-11 21:29:47 haley Exp $
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

      SUBROUTINE CFWRIT(IUNIT,ITYP,IOS,STATUS)
C  
C  Write the binary fontcap file.
C
C  INPUT
C	UNIT - The fortran logical unit number to be written to.
C       ITYP - = 0  for stroked font
C              = 1  for filled font
C
C  OUTPUT
C	IOS    - The I/O status word, IOS has meaning only if STATUS indicates
C                an error condition (i.e. if STATUS.GT.0).
C	STATUS - Integer-valued status indicator, see subroutine SFPRCF
C                for details.
C
      include 'fnterr.h'
      include 'capfnt.h'
      include 'fntcom.h'
C
      INTEGER IUNIT, IOS, STATUS
C
C  Set error STATUS to ALLOK
C
      STATUS = ALLOK
C
C  Write out the font description.
C
      IF (ITYP .EQ. 0) THEN
        CALL BINWRI(IUNIT, WRLEN, CHSTRT, IOS, STATUS)
      ELSE IF (ITYP .EQ. 1) THEN
        CALL BINWRI(IUNIT, NWRDS, BUFFER, IOS, STATUS)
      ENDIF
C
      RETURN
      END
