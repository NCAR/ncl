C
C	$Id: trnmch.f,v 1.3 2000-08-22 04:34:24 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
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
      SUBROUTINE TRNMCH
C
C  SET THE SYSTEM DEPENDENT CONSTANTS
C
      COMMON/TRINOT/ IPTR, MBUFOT, MAXBYT, DEVUNT, METUNT,
     1          METIPT, ERRUNT, FNTUNT
      INTEGER MAXCNT
      PARAMETER (MAXCNT=200)
      INTEGER IPTR, MBUFOT(MAXCNT), MAXBYT, DEVUNT, METUNT, ERRUNT,
     1        FNTUNT
      LOGICAL METIPT
      COMMON /TRMACH/ BTSWRD
      INTEGER BTSWRD
C
      INTEGER I1MACH
C
C  GET THE NUMBER OF BITS PER WORD
C
      BTSWRD = I1MACH(5)
C
C  COMPUTE THE NUMBER OF BYTES IN THE OUTPUT BUFFER
C
      MAXBYT = (BTSWRD/8) * MAXCNT
C
      RETURN
      END
