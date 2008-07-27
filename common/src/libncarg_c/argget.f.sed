C
C	$Id: argget.f.sed,v 1.2 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  2000
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE ARGGET(NUMBER,STRING)
C***********************************************************************
C	
C	Procedure: ARGGET()
C
C	Purpose: To determine the "NUMBER"th argument and
C		copy it into a CHARACTER*1 array "STRING".
C
C	Input Variables:
C		NUMBER:	The index of the desired argument.
C
C	Output Variables:
C		STRING:	A CHARACTER*1 array which is filled
C			with the appropriate argument.
C
C
C***********************************************************************
      INTEGER NUMBER
      CHARACTER*1 STRING(80)
      INTEGER I
      CHARACTER*80 ARG
C
      DO 10 I = 1,80
          ARG(I:I) = ' '
 10   CONTINUE

#ifdef	hpux
      CALL IGETARG(NUMBER,ARG, 80)
#else
      CALL GETARG(NUMBER,ARG)
#endif

      DO 20 I = 1,80
          STRING(I) = ARG(I:I)
 20   CONTINUE
C
      RETURN
      END
