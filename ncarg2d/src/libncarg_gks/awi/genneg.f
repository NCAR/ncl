C
C	$Id: genneg.f,v 1.2 2000-07-12 16:39:41 haley Exp $
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
      SUBROUTINE GENNEG(NUMIN,NUMOUT)
C
C  Where NUMIN has a 2's complement number in the low-order 16 bits,
C  this subroutine returns the equivalent number in the representation
C  of the host machine.
C
      NUMOUT = 0
      ISGN = IAND(ISHIFT(NUMIN,-15),1) 
C
C  If input is a positive number, set output to input.
C
      IF (ISGN .EQ. 0) THEN
        NUMOUT = NUMIN
        RETURN
      ELSE
C
C  Set NUMOUT from 1's complement plus 1.
C
        NUMTMP = IAND(32767,NUMIN)
        NUMTMP = 32767-NUMTMP
        NUMOUT = -(NUMTMP+1)
      ENDIF
      RETURN
C
      END
