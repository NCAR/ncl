C
C	$Id: garg.f,v 1.3 2000-08-22 04:34:21 haley Exp $
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
      SUBROUTINE GARG(ANUM,STRING,MAXSTR,STAT)
C
C  GET THE REQUESTED ARGUMENT
C
C  THERE ARE TWO WAYS TO DO THIS
C       1.USE A COMMAND LINE ARGUMENT EXTRACTOR WHICH IS SYSTEM
C         DEPENDENT AND MUST BE IMPLEMENTED AT EACH SITE.
C         IF THIS IS DONE THEN THE VARIABLE METIPT MUST BE SET TO
C         FALSE SO THIS SUBROUTINE WILL USE IT.
C       2.THE FORTRAN 77 READ STATEMENT
C
C  THE ARGUMENTS ARE
C       IN COMMAND LINE MODE THERE CAN BE NONE, ONE, OR TWO
C               0-THE F77 READ IS USED TO GET THE METAFILE NAME
C               1-THE METAFILE IS NAMED ON THE COMMAND LINE
C               2-THE RECORD NUMBER OF A FRAME TO BE PLOTTED IS GIVEN
C                       FOLLOWED BY THE NAME OF THE METAFILE TO USE
C       IN F77 MODE ONLY THE METAFILE NAME GIVEN
C
C  INPUT
C       ANUM-THE ARGUMENT NUMBER REQUESTED
C       MAXSTR-THE MAXIMUM NUMBER OF CHARACTERS TO TRANSFER
C  OUTPUT
C       STRING-CHARACTER STRING CONTAINING THE ARGUMENT
C       STAT-THE STATUS FLAG
C               0-ALL WENT OK
C               1-REQUESTED ARGUMENT DOES NOT EXIST
C               2-F77 READ USED (ONLY FILE NAME CAN BE GIVEN IN THIS MODE)
C
      COMMON/TRINOT/ IPTR, MBUFOT, MAXBYT, DEVUNT, METUNT,
     1          METIPT, ERRUNT, FNTUNT
      INTEGER MAXCNT
      PARAMETER (MAXCNT=200)
      INTEGER IPTR, MBUFOT(MAXCNT), MAXBYT, DEVUNT, METUNT, ERRUNT,
     1        FNTUNT
      LOGICAL METIPT
C
      INTEGER ANUM, MAXSTR, STAT
      CHARACTER*1 STRING(MAXSTR)
      INTEGER II, IARGCT, KK
C
C  INIT STRING TO BLANKS
C
      DO 11 II=1,MAXSTR
 11     STRING(II) = ' '
C
C  TEST IF F77 OR COMMAND LINE MODE
C
      IF (METIPT) THEN
C
C       PORTABLE FLAG ON READ FROM TERMINAL
C
        CALL WRITIT(' ENTER NAME OF METAFILE', 23)
        CALL READIT(STRING,MAXSTR)
        STAT = 2
      ELSE
C
C*******************************************************************
C  IF YOUR INSTALLATION HAS A COMMAND LINE READER THEN THIS IS WHERE
C  THE SUBROUTINE CALLS OCCUR. THE FUNCTION IARGCT RETURNS THE NUMBER
C  OF ARGUMENTS PASSED TO THE TRANSLATOR (1 PATH NAME  2 PATH NAME AND
C  RECORD START POINT).
C******************************************************************
C
C       COMMAND LINE READER TO BE USED IF THERE ARE ARGUMENTS
C
        IF (IARGCT(KK) .EQ. 0) THEN
C
C               NOTHING ON THE COMMAND LINE SO REQUEST THE FILE NAME
C
                CALL WRITIT(' ENTER NAME OF METAFILE', 23)
                CALL READIT(STRING,MAXSTR)
                STAT = 2
        ELSE
C
C               TEST IF THE REQUESTED COMMAND LINE ARG EXISTS
C
                IF (IARGCT(KK).GE.ANUM) THEN
C
C                       ALL OK SO GET IT
C
                        CALL ARGGET(ANUM,STRING)
                        STAT = 0
                ELSE
C
C                       ARG DOES NOT EXIST
C
                        STAT = 1
                END IF
        END IF
      END IF
C
      RETURN
C
      END
