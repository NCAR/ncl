C
C $Id: naskln.f,v 1.2 2000-07-13 02:49:00 haley Exp $
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
      SUBROUTINE NASKLN(IUNIT,LINE)
C
C  Return the next non-comment line on IUNIT in LINE.
C
      CHARACTER*80 LINE
C
    5 CONTINUE
      READ(IUNIT,100,END=200) LINE
  100 FORMAT(A80)
      IF (LINE(1:2) .EQ. '/*') GO TO 5
C
      RETURN
  200 CONTINUE
      WRITE(6,500)
  500 FORMAT(' Unexpected end of file on input')
      STOP
C
      END
