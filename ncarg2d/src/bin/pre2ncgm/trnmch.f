C
C	$Id: trnmch.f,v 1.4 2008-07-27 00:59:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TRNMCH
C
C  SET THE SYSTEM DEPENDENT CONSTANTS
C
      COMMON/TRINOT/ IPTR, MBUFOT, MAXBYT, DEVUNT, METUNT,
     1          METIPT, ERRUNT, FNTUNT
      INTEGER MAXCNT
      PARAMETER (MAXCNT=200)
      INTEGER IPTR, MBUFOT(MAXCNT), MAXBYT, DEVUNT, METUNT, ERRUNT,
     1        FNTUNT
      LOGICAL METIPT
      COMMON /TRMACH/ BTSWRD
      INTEGER BTSWRD
C
      INTEGER I1MACH
C
C  GET THE NUMBER OF BITS PER WORD
C
      BTSWRD = I1MACH(5)
C
C  COMPUTE THE NUMBER OF BYTES IN THE OUTPUT BUFFER
C
      MAXBYT = (BTSWRD/8) * MAXCNT
C
      RETURN
      END
