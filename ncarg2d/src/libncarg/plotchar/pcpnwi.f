C
C $Id: pcpnwi.f,v 1.5 2000-07-12 16:24:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
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
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      CHARACTER*16 FUNCTION PCPNWI (WHCH,IPAI)
C
      CHARACTER*(*) WHCH
      CHARACTER*16   CTMP
C
C Given the name (WHCH) of an internal parameter that is really an array
C and the integer value (IPAI) of the desired parameter array index, the
C value of PCPNWI (WHCH,IPAI) is the internal parameter name including
C the index, in the form expected by the routines PCGETR and PCSETR (in
C parentheses following the two-character internal parameter name).
C
      IF (LEN(WHCH).LT.2) THEN
        CALL SETER ('PCPNWI - PARAMETER NAME TOO SHORT',1,1)
        RETURN
      ELSE
        WRITE (CTMP,'(I16)') IPAI
        IBEG=0
        DO 101 I=1,16
          IF (CTMP(I:I).NE.' ') THEN
            IF (IBEG.EQ.0) IBEG=I
            IF (IBEG.NE.0) IEND=I
          END IF
  101   CONTINUE
        IF (IBEG.EQ.0) THEN
          CALL SETER ('PCPNWI - INTERNAL ERROR - SEE CONSULTANT',2,1)
          RETURN
        ELSE
          PCPNWI=WHCH(1:2)//'('//CTMP(IBEG:IEND)//')'//'            '
        END IF
      END IF
C
C Done.
C
      RETURN
C
      END
