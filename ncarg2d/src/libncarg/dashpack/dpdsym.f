C
C $Id: dpdsym.f,v 1.1 2004-11-16 21:30:32 kennison Exp $
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
      SUBROUTINE DPDSYM (XPMW,YPMW,CSYM,WCHW,ANGD)
C
        CHARACTER*1 CSYM
C
C This routine is called by DPDRAW to draw a symbol at the point
C (XPMW,YPMW) in the world coordinate system.  The symbol to be
C drawn is the one associated with the character CSYM.  Its width
C is to be WCHW, in the world coordinate system, and it is to be
C written at the angle ANGD, in degrees measured counterclockwise
C from a horizontal vector pointing to the right.
C
C Declare coordinate arrays in which to define the mark.
C
        DIMENSION XCRA(37),YCRA(37)
C
C Define a multiplicative constant to convert from degrees to radians.
C
        DATA DTOR / .017453292519943 /
C
C Compute the coordinates of the point defining the mark.  The curious
C constants used in some of this code are intended to make each symbol
C have the same area as that of a circle symbol.
C
        IF (CSYM.EQ.'0'.OR.CSYM.EQ.'5') THEN  !  circle
C
          DO 101 I=1,37
            ANGR=DTOR*(REAL(I-1)*10.)
            XCRA(I)=XPMW+(WCHW/2.)*COS(ANGR)
            YCRA(I)=YPMW+(WCHW/2.)*SIN(ANGR)
  101     CONTINUE
C
          NCRA=37
C
        ELSE IF (CSYM.EQ.'1'.OR.CSYM.EQ.'6') THEN  !  square
C
          DO 102 I=1,5
            ANGR=DTOR*(ANGD+45.+REAL(I-1)*90.)
            XCRA(I)=XPMW+1.25331*(WCHW/2.)*COS(ANGR)
            YCRA(I)=YPMW+1.25331*(WCHW/2.)*SIN(ANGR)
  102     CONTINUE
C
          NCRA=5
C
        ELSE IF (CSYM.EQ.'2'.OR.CSYM.EQ.'7') THEN  !  triangle
C
          DO 103 I=1,4
            ANGR=DTOR*(ANGD+90.+REAL(I-1)*120.)
            XCRA(I)=XPMW+1.55512*(WCHW/2.)*COS(ANGR)
            YCRA(I)=YPMW+1.55512*(WCHW/2.)*SIN(ANGR)
  103     CONTINUE
C
          NCRA=4
C
        ELSE IF (CSYM.EQ.'3'.OR.CSYM.EQ.'8') THEN  !  diamond
C
          DO 104 I=1,5
            ANGR=DTOR*(ANGD+REAL(I-1)*90.)
            XCRA(I)=XPMW+1.25331*(WCHW/2.)*COS(ANGR)
            YCRA(I)=YPMW+1.25331*(WCHW/2.)*SIN(ANGR)
  104     CONTINUE
C
          NCRA=5
C
        ELSE IF (CSYM.EQ.'4'.OR.CSYM.EQ.'9') THEN  !  star
C
          DO 105 I=1,11
            ANGR=DTOR*(ANGD+18.+REAL(I-1)*36.)
            IF (MOD(I,2).EQ.0) THEN
              RADI=.381966
            ELSE
              RADI=1.
            END IF
            XCRA(I)=XPMW+1.67289*RADI*(WCHW/2.)*COS(ANGR)
            YCRA(I)=YPMW+1.67289*RADI*(WCHW/2.)*SIN(ANGR)
  105     CONTINUE
C
          NCRA=11

        END IF
C
C Fill or outline the mark.
C
        IF (CSYM.GE.'0'.AND.CSYM.LE.'4') THEN
          CALL GFA (NCRA,XCRA,YCRA)
        ELSE
          CALL GPL (NCRA,XCRA,YCRA)
        END IF
C
C Done.
C
        RETURN
C
      END
