C
C $Id: displa.f,v 1.5 2006-03-09 22:56:09 kennison Exp $
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
      SUBROUTINE DISPLA (LFRA,LROW,LTYP)
C
C The subroutine DISPLA resets the parameters IFRA, IROW, and/or LLUX
C and LLUY.
C
      IF (LFRA.NE.0) CALL AGSETI ('FRAM.', MAX(1,MIN(3,LFRA)))
C
      IF (LROW.NE.0) CALL AGSETI ('ROW .',LROW)
C
      IF (LTYP.EQ.0) RETURN
C
      ITYP=MAX(1,MIN(4,LTYP))
      CALL AGSETI ('X/LOGA.',   (1-ITYP)/2)
      CALL AGSETI ('Y/LOGA.',MOD(1-ITYP,2))
C
      RETURN
C
      END
