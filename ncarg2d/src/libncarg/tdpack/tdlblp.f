C
C $Id: tdlblp.f,v 1.1 2005-08-23 22:08:34 kennison Exp $
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
      SUBROUTINE TDLBLP (AXIS,VLBL,PLBL)
C
        CHARACTER*1 AXIS
C
C AXIS is a single alphabetic character copied from the beginning of a
C string containing numeric labels to be used on a particular axis; it
C specifies how the label value passed in VLBL is to be used to compute
C a label position to be returned in PLBL.  The default version just
C puts the labels where their numeric values indicate they should go,
C but a user version may be supplied to modify the positions of numeric
C labels along any of the axes more or less at will.
C
        IF      (AXIS.EQ.'U') THEN
          PLBL=VLBL
        ELSE IF (AXIS.EQ.'V') THEN
          PLBL=VLBL
        ELSE IF (AXIS.EQ.'W') THEN
          PLBL=VLBL
        ELSE
          PLBL=VLBL
        END IF
C
        RETURN
C
      END
