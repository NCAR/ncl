C
C $Id: mplndm.f,v 1.12 2001-08-16 23:10:51 kennison Exp $
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
      SUBROUTINE MPLNDM (FLNM,ILVL,IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,
     +                                                           ULPR)
        CHARACTER*(*) FLNM
        INTEGER       ILVL,IAMA(*)
        REAL          XCRA(MCRA),YCRA(MCRA)
        INTEGER       MCRA,IAAI(MNOG),IAGI(MNOG),MNOG
        IF (ICFELL('MPLNDM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDLNDM (FLNM,ILVL,IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
        RETURN
      END
