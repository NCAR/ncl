C
C	$Id: gtnumb.f,v 1.4 2008-07-27 00:17:21 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
