C
C	$Id: gtnum.f,v 1.4 2008-07-27 00:17:21 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
