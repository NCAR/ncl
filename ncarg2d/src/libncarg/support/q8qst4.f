C
C	$Id: q8qst4.f,v 1.4 2008-07-27 00:17:31 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE Q8QST4(NAME,LBRARY,ENTRY,VRSION)
C
C DIMENSION OF           NAME(1),LBRARY(1),ENTRY(1),VRSION(1)
C ARGUMENTS
C
C LATEST REVISION        MARCH 1984
C
C PURPOSE                MONITORS LIBRARY USE BY WRITING A RECORD WITH
C                        INFORMATION ABOUT THE CIRCUMSTANCES OF A
C                        LIBRARY ROUTINE CALL TO THE SYSTEM ACCOUNTING
C                        TAPE FOR LATER PROCESSING.
C
C NOTE---                THIS VERSION OF Q8QST4 SIMPLY RETURNS TO THE
C                        CALLING ROUTINE.  LOCAL IMPLEMENTORS MAY WISH
C                        TO IMPLEMENT A VERSION OF THIS ROUTINE THAT
C                        MONITORS USE OF NCAR ROUTINES WITH LOCAL
C                        MECHANISMS.  OTHERWISE IT WILL SAVE A SMALL
C                        AMOUNT OF SPACE AND TIME IF CALLS TO Q8QST4 ARE
C                        DELETED FROM ALL NSSL ROUTINES.
C
      CHARACTER*(*) NAME,LBRARY,ENTRY,VRSION
C
      RETURN
      END
