C
C $Id: agfpbn.f,v 1.3 2000-07-12 16:21:57 haley Exp $
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
      INTEGER FUNCTION AGFPBN (FPDP)
C
C The value of AGFPBN(FPDP) is a binary dash pattern, obtained from the
C floating-point dash pattern FPDP.  On machines having a word length
C greater than 16 bits, AGFPBN(FPDP) = IFIX(FPDP).  On machines having
C a word length of 16 bits, this is not true.  For example, when FPDP =
C 65535. (2 to the 16th minus 1), the equivalent binary dash pattern
C does not have the value 65535, but the value -1 (assuming integers
C are represented in a ones' complement format).  So, the functions
C ISHIFT and IOR must be used to generate the dash pattern.
C
      TEMP=FPDP
      AGFPBN=0
C
      DO 101 I=1,16
        IF (AMOD(TEMP,2.).GE.1.) AGFPBN=IOR(AGFPBN,ISHIFT(1,I-1))
        TEMP=TEMP/2.
  101 CONTINUE
C
      RETURN
C
      END
