C
C $Id: tdgrid.f,v 1.3 2000-08-22 15:07:08 haley Exp $
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
      SUBROUTINE TDGRID (XBEG,XSTP,NOXS,YBEG,YSTP,NOYS,IGRD)
C
C The routine TDGRID is called to draw a grid in the parallelogram
C defined by the last call to TDPARA.  The arguments are as follows:
C
C   XBEG, XSTP, and NOXS define where ticks or grid lines are to be
C   drawn along the "X" axis of the parallelogram (at XBEG, XBEG+XSTP,
C   XBEG+2*XSTP, ... XBEG+NOXS*XSTP).
C
C   YBEG, YSTP, and NOYS define where ticks or grid lines are to be
C   drawn along the "Y" axis of the parallelogram (at YBEG, YBEG+YSTP,
C   YBEG+2*YSTP, ... YBEG+NOYS*YSTP).
C
C   IGRD defines what is to be drawn and has one of the values 1 (draw
C   just a perimeter), 2 (draw a perimeter with inward-pointing ticks),
C   or 3 (draw a perimeter with a grid).
C
C The code is pretty straightforward ...
C
        IF (IGRD.GT.0) THEN
          CALL TDLNPA (0.,0.,1.,0.)
          CALL TDLNPA (1.,0.,1.,1.)
          CALL TDLNPA (1.,1.,0.,1.)
          CALL TDLNPA (0.,1.,0.,0.)
          IF (IGRD.EQ.2) THEN
            DO 101 I=1,NOXS
              CALL TDLNPA (XBEG+REAL(I-1)*XSTP,0.0,
     +                     XBEG+REAL(I-1)*XSTP,.02)
              CALL TDLNPA (XBEG+REAL(I-1)*XSTP,1.0,
     +                     XBEG+REAL(I-1)*XSTP,.98)
  101       CONTINUE
            DO 102 J=1,NOYS
              CALL TDLNPA (0.0,YBEG+REAL(J-1)*YSTP,
     +                     .02,YBEG+REAL(J-1)*YSTP)
              CALL TDLNPA (1.0,YBEG+REAL(J-1)*YSTP,
     +                     .98,YBEG+REAL(J-1)*YSTP)
  102       CONTINUE
          ELSE IF (IGRD.EQ.3) THEN
            DO 103 I=1,NOXS
              CALL TDLNPA (XBEG+REAL(I-1)*XSTP,0.,
     +                     XBEG+REAL(I-1)*XSTP,1.)
  103       CONTINUE
            DO 104 J=1,NOYS
              CALL TDLNPA (0.,YBEG+REAL(J-1)*YSTP,
     +                     1.,YBEG+REAL(J-1)*YSTP)
  104       CONTINUE
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
