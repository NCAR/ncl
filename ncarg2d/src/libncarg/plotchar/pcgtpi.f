C
C $Id: pcgtpi.f,v 1.9 2000-07-12 16:24:58 haley Exp $
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
      SUBROUTINE PCGTPI (CHRS,NCHR,ICHR,IBSE,NUMV)
C
C The subroutine PCGTPI gets a positive integer from the character
C string CHRS.  Its arguments are as follows:
C
C   CHRS is an input variable.  It contains a character string.
C
C   NCHR is an input variable specifying the number of characters in
C   CHRS.
C
C   ICHR is both an input variable and an output variable.  On entry,
C   it contains the index of the character in CHRS at which a positive
C   integer may start.  On exit, it contains the index of the character
C   following the last character of the integer, if one was found;
C   otherwise, it is unchanged.
C
C   IBSE is an input variable, specifying the base value to be used in
C   computing the value of the integer - either 8 or 10.
C
C   NUMV is an output variable containing the value of the positive
C   integer found.  The value zero indicates that no such integer was
C   found.
C
C COMMON block declarations.
C
      COMMON /PCSVEM/ ICOD,IDDA(8625),IDDL,RDGU(7000),IDPC(256),IERU,
     +                INDA(789),INDL,INIT,IVCO,IVDU,NBPW,NPPW
      SAVE   /PCSVEM/
C
      CHARACTER*(*) CHRS
C
C Initialize the value to be returned.
C
      NUMV=0
C
C Get the DPC value of the character '0' in the collating sequence.
C
      IZRO=IDPC(ICHAR('0'))
C
C Examine each character, updating the value of the number and the
C position of the first possible non-digit, until a non-digit is found.
C
      IMAX=NCHR-ICHR+1
      DO 101 I=1,IMAX
        IDGT=IDPC(ICHAR(CHRS(ICHR:ICHR)))-IZRO
        IF (IDGT.GE.0.AND.IDGT.LT.IBSE) THEN
          NUMV=IBSE*NUMV+IDGT
          ICHR=ICHR+1
        ELSE
          RETURN
        END IF
 101  CONTINUE
C
C Done.
C
      RETURN
C
      END
