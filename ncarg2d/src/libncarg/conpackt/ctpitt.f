C
C $Id: ctpitt.f,v 1.1 2004-03-19 22:51:57 kennison Exp $
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
      SUBROUTINE CTPITT (XCA1,YCA1,XCA2,YCA2,XCA3,YCA3,
     +                   XCB1,YCB1,XCB2,YCB2,XCB3,YCB3,
     +                   XCOP,YCOP,IFLG)
C
C Given the coordinates of the corner points of two triangles in the
C plane, this routine computes and returns the coordinates of a point
C in both triangles, if such a point exists.  If no such point exists,
C IFLG is returned non-zero; otherwise, it is a count of the number of
C points used to compute the common point.
C
C Declare an arithmetic statement function that has one sign if point
C 3 is to the left of the line from point 1 to point 2 and a different
C sign if point 3 is to the right of that line.
C
      SIDE(X1,Y1,X2,Y2,X3,Y3)=(X1-X3)*(Y2-Y3)-(Y1-Y3)*(X2-X3)
C
C Initialize the quantities to be returned.
C
      XCOP=0.
      YCOP=0.
      IFLG=0
C
C Use any point of A that is inside B.
C
      TMP1=SIDE(XCB1,YCB1,XCB2,YCB2,XCA1,YCA1)
      TMP2=SIDE(XCB2,YCB2,XCB3,YCB3,XCA1,YCA1)
      TMP3=SIDE(XCB3,YCB3,XCB1,YCB1,XCA1,YCA1)
C
      IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.(TMP1.GT.0..AND.
     +TMP2.GT.0..AND.TMP3.GT.0.)) THEN
        XCOP=XCOP+XCA1
        YCOP=YCOP+YCA1
        IFLG=IFLG+1
      END IF
C
      TMP1=SIDE(XCB1,YCB1,XCB2,YCB2,XCA2,YCA2)
      TMP2=SIDE(XCB2,YCB2,XCB3,YCB3,XCA2,YCA2)
      TMP3=SIDE(XCB3,YCB3,XCB1,YCB1,XCA2,YCA2)
C
      IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.(TMP1.GT.0..AND.
     +TMP2.GT.0..AND.TMP3.GT.0.)) THEN
        XCOP=XCOP+XCA2
        YCOP=YCOP+YCA2
        IFLG=IFLG+1
      END IF
C
      TMP1=SIDE(XCB1,YCB1,XCB2,YCB2,XCA3,YCA3)
      TMP2=SIDE(XCB2,YCB2,XCB3,YCB3,XCA3,YCA3)
      TMP3=SIDE(XCB3,YCB3,XCB1,YCB1,XCA3,YCA3)
C
      IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.(TMP1.GT.0..AND.
     +TMP2.GT.0..AND.TMP3.GT.0.)) THEN
        XCOP=XCOP+XCA3
        YCOP=YCOP+YCA3
        IFLG=IFLG+1
      END IF
C
C Use any point of B that is inside A.
C
      TMP1=SIDE(XCA1,YCA1,XCA2,YCA2,XCB1,YCB1)
      TMP2=SIDE(XCA2,YCA2,XCA3,YCA3,XCB1,YCB1)
      TMP3=SIDE(XCA3,YCA3,XCA1,YCA1,XCB1,YCB1)
C
      IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.(TMP1.GT.0..AND.
     +TMP2.GT.0..AND.TMP3.GT.0.)) THEN
        XCOP=XCOP+XCB1
        YCOP=YCOP+YCB1
        IFLG=IFLG+1
      END IF
C
      TMP1=SIDE(XCA1,YCA1,XCA2,YCA2,XCB2,YCB2)
      TMP2=SIDE(XCA2,YCA2,XCA3,YCA3,XCB2,YCB2)
      TMP3=SIDE(XCA3,YCA3,XCA1,YCA1,XCB2,YCB2)
C
      IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.(TMP1.GT.0..AND.
     +TMP2.GT.0..AND.TMP3.GT.0.)) THEN
        XCOP=XCOP+XCB2
        YCOP=YCOP+YCB2
        IFLG=IFLG+1
      END IF
C
      TMP1=SIDE(XCA1,YCA1,XCA2,YCA2,XCB3,YCB3)
      TMP2=SIDE(XCA2,YCA2,XCA3,YCA3,XCB3,YCB3)
      TMP3=SIDE(XCA3,YCA3,XCA1,YCA1,XCB3,YCB3)
C
      IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.(TMP1.GT.0..AND.
     +TMP2.GT.0..AND.TMP3.GT.0.)) THEN
        XCOP=XCOP+XCB3
        YCOP=YCOP+YCB3
        IFLG=IFLG+1
      END IF
C
C Use all points of intersection of the edges.
C
      CALL CTPISB (XCA1,YCA1,XCA2,YCA2,XCB1,YCB1,XCB2,YCB2,
     +                                      XCOP,YCOP,IFLG)
      CALL CTPISB (XCA1,YCA1,XCA2,YCA2,XCB2,YCB2,XCB3,YCB3,
     +                                      XCOP,YCOP,IFLG)
      CALL CTPISB (XCA1,YCA1,XCA2,YCA2,XCB3,YCB3,XCB1,YCB1,
     +                                      XCOP,YCOP,IFLG)
      CALL CTPISB (XCA2,YCA2,XCA3,YCA3,XCB1,YCB1,XCB2,YCB2,
     +                                      XCOP,YCOP,IFLG)
      CALL CTPISB (XCA2,YCA2,XCA3,YCA3,XCB2,YCB2,XCB3,YCB3,
     +                                      XCOP,YCOP,IFLG)
      CALL CTPISB (XCA2,YCA2,XCA3,YCA3,XCB3,YCB3,XCB1,YCB1,
     +                                      XCOP,YCOP,IFLG)
      CALL CTPISB (XCA3,YCA3,XCA1,YCA1,XCB1,YCB1,XCB2,YCB2,
     +                                      XCOP,YCOP,IFLG)
      CALL CTPISB (XCA3,YCA3,XCA1,YCA1,XCB2,YCB2,XCB3,YCB3,
     +                                      XCOP,YCOP,IFLG)
      CALL CTPISB (XCA3,YCA3,XCA1,YCA1,XCB3,YCB3,XCB1,YCB1,
     +                                      XCOP,YCOP,IFLG)
C
C Average over all points found; return average X and average Y.
C
      IF (IFLG.NE.0) THEN
        XCOP=XCOP/REAL(IFLG)
        YCOP=YCOP/REAL(IFLG)
      END IF
C
C Done.
C
      RETURN
C
      END
