C
C	$Id: gtnumb.f,v 1.3 2000-08-22 15:05:43 haley Exp $
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
C
C============================================================
C                       GTNUMB
C============================================================
      SUBROUTINE GTNUMB (IDPC,N,II,NUM)
C
C
C EXTRACTS AN OCTAL NUMBER NUM FROM CHARACTER STRING IDPC.
C
C ON ENTRY:
C   IDPC - A CHARACTER STRING
C   N - THE NUMBER OF CHARACTERS IN THE CHARACTER STRING IDPC
C   II - A NUMBER REPRESENTING A POINTER TO THE II-TH CHARACTER IN THE
C        CHARACTER STRING IDPC
C        THE II-TH CHARACTER HAS TO BE A DIGIT BETWEEN 0 AND 7
C   NUM - CAN HAVE ANY VALUE
C
C ON EXIT:
C   IDPC - UNCHANGED
C   N    - UNCHANGED
C   NUM  - 1) IF THE II-TH CHARACTER IN IDPC IS THE FIRST CHARACTER
C             IN A SUBSTRING REPRESENTING AN UNSIGNED INTEGER, THEN THE
C             VALUE OF THIS UNSIGNED INTEGER TAKEN TO THE BASE 8
C            (NOTICE THE DIFFERENCE FROM SUBROUTINE GTNUM)
C          2) OTHERWISE 0
C   II   - 1)IF AN UNSIGNED INTEGER WAS FOUND THEN II REPRESENTS A POINT
C            TO THE CHARACTER SUCCEEDING THE LAST CHARACTER OF THE
C            UNSIGNED INTEGER
C          2) OTHERWISE UNCHANGED
C
C CALLS
C   GTDGTS
C
C CALLED BY
C   PWRITX
C
C
C ASSUMPTION
C   THE REPRESENTATION OF DIGITS IN THE MACHINE IS ORDERED AND COMPACT.
C
C
      CHARACTER IDPC*(*)
C
C
C **********************************************************************
C
C LOCAL VARIABLES
C
C ID - A character representing the base of the number to be extracted
C
C **********************************************************************
C
C
C THE BASE OF THE NUMBER TO BE EXTRACTED.
C
      CHARACTER*1 ID
      ID =  '8'
C
      CALL GTDGTS(IDPC,II,N,ID,NUM)
C                 Convert digit string beginning at II in IDPC
C                 to an integer, base ID.
C
      RETURN
      END
