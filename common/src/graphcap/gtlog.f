C
C	$Id: gtlog.f,v 1.2 2000-07-11 21:31:58 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this library; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C

      SUBROUTINE GTLOG(STRING, STRMAX, STRSIZ, IOS, STATUS)
C
C  Get a string of logical values from the input file.
C
C  INPUT
C       STRMAX  --  Maximum number of logical values to transfer.
C
C  OUTPUT
C       STRING  --  The array of logical values.
C       STRSIZ  --  The number of logical values actually transfered.
C       IOS     --  I/O status flag.  This flag is valid only
C                   if STATUS indicates an error.
C       STATUS  --  The error status as defined in COMMON CAPERR.
C
      COMMON /CAPERR/ ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1                FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR,
     2                UNDASC, DEVERR, DOCERR, TBLERR , STSZER, ENTERR,
     3                TABERR, TABSER, PRSFIL
      INTEGER ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1        FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR, UNDASC,
     2        DEVERR, DOCERR, STSZER, ENTERR, TABERR, TABSER, TBLERR,
     3        PRSFIL
      COMMON /CAPIOB/ UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPIO2/ LINE, GRNM
      INTEGER LNMAX
      PARAMETER (LNMAX=80)
      CHARACTER*80 LINE
      CHARACTER*80 GRNM
      INTEGER UNIT, IPTR, LSIZE, NFST, NFLG
C
      INTEGER STRMAX, STRSIZ, IOS, STATUS
      LOGICAL STRING(STRMAX)
      INTEGER LL, II
      LOGICAL TEMP
      CHARACTER*1 T(4),F(5)
C
C  Define the true and false strings.
C
      DATA T /'T','R','U','E'/
      DATA F /'F','A','L','S','E'/
C
C  Set STRING size to null.
C
      STRSIZ = 0
C
C  Skip blanks.
C
 10   CONTINUE
      CALL SKPBLK(IOS, STATUS)
      IF (STATUS .NE. ALLOK) RETURN
      LL = 1
C
C  Test for a true.
C
      DO 20 II = IPTR,LSIZE
        IF (LINE(II:II) .NE. T(LL)) GO TO 30
        LL = LL + 1
        IF (LL .EQ. 5) THEN
C
C  True found.
C
                TEMP = .TRUE.
                IPTR = II + 1
                GO TO 40
        END IF
 20   CONTINUE
C
C  Test for a false.
C
 30   CONTINUE
      LL = 1
      DO 35 II = IPTR,LSIZE
        IF (LINE(II:II) .NE. F(LL)) RETURN
        LL = LL + 1
        IF (LL .EQ. 6) THEN
C
C  False found.
C
                TEMP = .FALSE.
                IPTR = II + 1
                GO TO 40
        END IF
 35   CONTINUE
C
C  Store the value found in STRING.
C
 40   CONTINUE
      STRSIZ = STRSIZ + 1
      STRING(STRSIZ) = TEMP
      IF (STRSIZ .GE. STRMAX) RETURN
      GO TO 10
      END
