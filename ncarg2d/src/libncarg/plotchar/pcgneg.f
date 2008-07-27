C
C	$Id: pcgneg.f,v 1.5 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCGNEG (NUMIN,NUMOUT)
C
C  Where NUMIN has a 2's complement number in the low-order 16 bits,
C  this subroutine returns the equivalent number in the representation
C  of the host machine.
C
      NUMOUT = 0
      ISGN = IAND(ISHIFT(NUMIN,-15),1)
C
C  If input is a positive number, set output to input.
C
      IF (ISGN .EQ. 0) THEN
        NUMOUT = NUMIN
        RETURN
      ELSE
C
C  Set NUMOUT from 1's complement plus 1.
C
        NUMTMP = IAND(32767,NUMIN)
        NUMTMP = 32767-NUMTMP
        NUMOUT = -(NUMTMP+1)
      ENDIF
      RETURN
C
      END
