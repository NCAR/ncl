C
C $Id: arini2.f,v 1.11 2000-07-12 16:21:47 haley Exp $
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
      SUBROUTINE ARINI2 (ILC,RS1,RS2,RS3,DS1,DS2,DS3,RS4,RS5,RS6)
C
      DOUBLE PRECISION DS1,DS2,DS3
C
C This code had to be moved here from the routine ARINIT in order to
C force compilers (on the Mac and possibly elsewhere) to set up code
C that not only computes these quantities to the stated precision,
C but actually uses the values computed and stored, instead of using
C values from extended-precision arithmetic registers.
C
      RS1=REAL(ILC)*REAL(ILC)
      RS2=RS1+.25E0
      RS3=RS2+.25E0
      DS1=DBLE(ILC)*DBLE(ILC)
      DS2=DS1+.25D0
      DS3=DS2+.25D0
      RS4=REAL(ILC)
      RS5=RS4+.25E0
      RS6=RS5+.25E0
C
C Done.
C
      RETURN
C
      END
