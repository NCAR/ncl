C
C	$Id: gceldc.f,v 1.5 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GCELDC(OPRNS,OPNLEN,COUNT,IOS,STATUS)
C
C  This subroutine will extract the requested number of operands from
C  the segment and decode them according to the required format.
C  It will cross partition boundries without intervention from a 
C  higher level.
C
C  The LEN variable which higher level routines utilize for knowlage of
C  the size and remainder in the partition is modified.
C
C  INPUT
C    OPNLEN --  Length of each operand in bits.
C    COUNT  --  Number of operands to fetch.
C
C  OUTPUT
C    OPRNS  --  Array of operands in proper format.
C
      include 'trinst.h'
C
      INTEGER OPNLEN, COUNT, OPRNS(COUNT), IOS, STATUS
      INTEGER BYTSIZ, ZERO, BCOUNT, TCOUNT, SCOUNT, BYTLFT, TOMANY
C
C  Define the 8 bit byte size.
C
      DATA BYTSIZ/8/,ZERO/0/
C
      TOMANY = 100
      STATUS = 0
C
      BYTLFT = (OPNLEN*COUNT)/BYTSIZ
      BCOUNT = 1
      TCOUNT = COUNT
 10   CONTINUE
C
C  Determine the number of bytes left in the partition.
C
      IF (BYTLFT .GT. LEN) THEN
C
C  Not enough room left in the partition.
C
        SCOUNT = LEN/(OPNLEN/BYTSIZ)
        TCOUNT = TCOUNT - SCOUNT
        LEN = 0
      ELSE
C
C  Room for all the remaining operands.
C
        SCOUNT = TCOUNT
        TCOUNT = ZERO
        LEN = LEN -(SCOUNT*(OPNLEN/BYTSIZ))
      END IF
C
C  Get the operands.
C
      CALL GOPDEC(OPRNS(BCOUNT),OPNLEN,SCOUNT,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Get the next instruction partition if necessary.
C
      IF (TCOUNT .NE. ZERO) THEN
        IF (CNTINU) THEN
          CALL GNPART(IOS,STATUS)
          IF (STATUS .NE. 0) RETURN
        ELSE
          STATUS = TOMANY
          RETURN
        END IF
C
        BCOUNT = BCOUNT + SCOUNT
        BYTLFT = (OPNLEN*TCOUNT)/BYTSIZ
        GO TO 10
      END IF
C
      RETURN
      END
