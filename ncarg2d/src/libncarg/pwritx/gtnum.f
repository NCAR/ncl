C
C	$Id: gtnum.f,v 1.3 2000-08-22 15:05:43 haley Exp $
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
C============================================================
C                       GTNUM
C============================================================
      SUBROUTINE GTNUM (IDPC,N,II,NUM)
C
C EXTRACTS AN OPTIONALLY SIGNED INTEGER FROM CHARACTER STRING IDPC.
C
C ON ENTRY:
C   IDPC - A character string
C   N -    The number of characters in IDPC
C   II -   Pointer to the character preceding a possible integer
C   NUM -  can have any value
C
C ON EXIT:
C   IDPC - UNCHANGED
C   N    - UNCHANGED
C   NUM  - 1) IF THE (II+1)-TH CHARACTER IN IDPC IS THE FIRST CHARACTER
C             IN A SUBSTRING REPRESENTING A SIGNED INTEGER, THEN THE
C             VALUE OF THIS INTEGER
C          2) OTHERWISE 0
C   II   - 1) IF AN INTEGER WAS FOUND THEN II POINTS
C             TO THE LAST CHARACTER OF THE INTEGER
C          2) OTHERWISE  unchanged
C
C CALLS
C   GTSIGN
C   GTDGTS
C
C CALLED BY
C   PWRITX
C
C ASSUMPTION
C   THE REPRESENTATION OF DIGITS IN THE MACHINE IS ORDERED AND COMPACT.
C
C
      CHARACTER*(*) IDPC
C
C
C **********************************************************************
C
C LOCAL VARIABLES
C
C BASE   - A string representing the base of the number to be extracted
C ISIN =  1  Indicates the integer is positive ( sign = '+' or no sign)
C      = -1  Indicates the integer began with a negative sign
C
C **********************************************************************
C
C
C THE BASE OF THE NUMBER TO BE EXTRACTED.
C
      CHARACTER*2 BASE
      BASE = '10'
C
      ISIN = 1
      NUM  = 0
C                    Point to the first possible sign or digit
      II = II + 1
C                    If at least two characters, check for a sign
      IF (II .LT. N) THEN
           CALL GTSIGN(IDPC,II,ISIN)
      ENDIF
C                    If it still may be a number, find the value of
C                    the string of digits.
      IF (ISIN .NE. 0) THEN
           CALL GTDGTS(IDPC,II,N,BASE,NUM)
      ENDIF
C
C                    Compute the value of the signed number
      NUM = NUM*ISIN
C                    Point to the last digit or reset to initial value
      II = II - 1
C
C
      RETURN
      END
