C
C $Id: tdgrid.f,v 1.4 2008-07-27 00:17:32 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
