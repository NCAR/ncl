C
C	$Id: gethol.f,v 1.2 2000-07-12 16:25:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
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
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
C
      SUBROUTINE GETHOL (INCHAR,IOUTCH,IERR)
C ON ENTRY
C   INCHAR is a single FORTRAN 77 type character.
C   IOUTCH AND IERR MAY HAVE ANY VALUE.
C   Subroutine HTABLE must have already been called
C ON EXIT
C   INCHAR IS UNCHANGED.
C   IOUTCH CONTAINS THE REPRESENTATION OF THE CHARACTER IN INCHAR IN
C   HOLLERITH CODE, RIGHT JUSTIFIED.
C   IOUTCH CONTAINS 200 IF INCHAR DID NOT REPRESENT A FORTRAN CHARACTER
C   OR AN APOSTROPHE.
C   IERR = 0 IF THE CHARACTER IN INCHAR WAS REALLY IN THE FORTRAN
C            CHARACTER SET OR AN APOSTROPHE.
C        = 1 OTHERWISE
C
      INTEGER     TBSIZE, IOUTCH, HOLLER, IERR
      PARAMETER (TBSIZE=256)
C
C If your machine uses the ASCII character set, TBSIZE can be changed
C to 128.
C
      COMMON /HOLTAB/ HOLLER(TBSIZE), READY
      LOGICAL     READY
      CHARACTER*1 INCHAR
C
C This routine depends on the fact that the HOLLER array has been
C initialized by a call to HTABLE
C
      IF ( .NOT. READY) THEN
           IERR   = 1
           IOUTCH = 200
           GOTO 999
      ENDIF
C
      IERR   = 0
C
C Initialize the error indicator to 'no error', then retrieve the
C value from the hollerith code table using the intrinsic ICHAR
C function as an index into the table.
C
      IOUTCH = HOLLER ( ICHAR(INCHAR) )
      IF (IOUTCH .EQ. 200) IERR = 1
C
 999  CONTINUE
      RETURN
      END
