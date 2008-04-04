C
C $Id: mdrgsc.f,v 1.4 2008-04-04 21:02:47 kennison Exp $
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
      SUBROUTINE MDRGSC (JCOL,JCSF)
C
C This routine may be called to set the values of two arrays in EZMAP,
C one of which determines the colors to be used for outline maps drawn
C using the RANGS/GSHHS data, and the other of which determines the
C colors to be used for solid-filled maps drawn using that data.
C
        INTEGER JCOL(5),JCSF(5)
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
C Declare local variables.
C
        INTEGER          I
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDRGSC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer contents of color-index argument arrays to internal arrays.
C
        DO 101 I=1,5
          ICOL(I)=JCOL(I)
          ICSF(I)=JCSF(I)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
