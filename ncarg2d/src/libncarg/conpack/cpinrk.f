C
C $Id: cpinrk.f,v 1.6 2000-07-12 16:22:30 haley Exp $
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
      SUBROUTINE CPINRK (NSDR,TMP1,TMP2,TMP3)
C
C This routine computes some quantities needed by CPINRC; the code is
C here so as to ensure that, on machines on which arithmetic is done
C in double-precision registers, these quantities will be truncated to
C real precision before being used in tests.
C
      TMP1=10.**(-NSDR)
      TMP2=  1.+TMP1
      TMP3=TMP2+TMP1
C
C Done.
C
      RETURN
C
      END
