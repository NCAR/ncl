C
C $Id: aggetc.f,v 1.5 2006-03-09 22:56:05 kennison Exp $
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
      SUBROUTINE AGGETC (TPID,CUSR)
C
      CHARACTER*(*) TPID,CUSR
C
      DIMENSION FURA(1)
C
C The routine AGGETC is used to get the character strings represented
C by the values of certain individual AUTOGRAPH parameters.  TPID is a
C parameter identifier (from the caller).  CUSR is a character string
C (returned to the caller).
C
C See what kind of parameter is being gotten.
C
      CALL AGCTCS (TPID,ITCS)
C
C If the parameter is not intrinsically of type character, log an error.
C
      IF (ITCS.EQ.0) GO TO 901
C
C Otherwise, get the integer value of the parameter and use that to get
C the desired character string.
C
      CALL AGGETP (TPID,FURA,1)
      CALL AGGTCH (INT(FURA(1)),CUSR,LNCS)
C
C Done.
C
      RETURN
C
C Error exit.
C
  901 CALL AGPPID (TPID)
      CALL SETER ('AGGETC - PARAMETER TO GET IS NOT INTRINSICALLY OF TYP
     +E CHARACTER',2,2)
C
      END
