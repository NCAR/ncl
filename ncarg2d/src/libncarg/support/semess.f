C
C $Id: semess.f,v 1.4 2000-08-22 15:06:58 haley Exp $
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
      FUNCTION SEMESS (ITRIM)
C
        CHARACTER*113 SEMESS
C
C The value of this function is some portion of the current error
C message (blanks if there is no current error).  If the value of
C ITRIM is less than or equal to zero, the entire error message is
C returned.  If the value of ITRIM is equal to 1, prepended routine
C names and associated slashes are removed.  If the value of ITRIM
C is 2 or greater, all routine names are removed, along with the
C following hyphen and the blanks around the hyphen.  For example,
C if the current error message is
C
C   'CPCLAM/AREDAM - AREA-MAP ARRAY OVERFLOW'
C
C then
C
C   SEMESS(0)='CPCLAM/AREDAM - AREA-MAP ARRAY OVERFLOW'
C   SEMESS(1)='AREDAM - AREA-MAP ARRAY OVERFLOW'
C   SEMESS(2)='AREA-MAP ARRAY OVERFLOW'
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDA.
C
        COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
        SAVE   /SECOMI/
C
        COMMON /SECOMC/ ERMSG
          CHARACTER*256 ERMSG
        SAVE   /SECOMC/
C
C Return either blanks or the requested portion of the current message.
C
        IF (IERRF.EQ.0) THEN
          SEMESS=' '
        ELSE IF (ITRIM.LE.0) THEN
          SEMESS=ERMSG(1:LOMSG)
        ELSE
          IBEG=1
          IF (ITRIM.EQ.1) THEN
            DO 101 I=1,LOMSG
              IF (ERMSG(I:I).EQ.'/') IBEG=I+1
              IF (ERMSG(I:I).EQ.'-') GO TO 103
  101       CONTINUE
          ELSE
            DO 102 I=1,LOMSG
              IF (ERMSG(I:I).EQ.'-') THEN
                IBEG=I+2
                GO TO 103
              END IF
  102       CONTINUE
          END IF
  103     IF (IBEG.GT.LOMSG) IBEG=1
          SEMESS=ERMSG(IBEG:LOMSG)
        END IF
C
C Done.
C
        RETURN
C
      END
