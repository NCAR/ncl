C
C $Id: itrovr.f,v 1.5 2008-07-27 00:17:31 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION ITROVR (XCP1,YCP1,XCP2,YCP2,XCP3,YCP3,
     +                 XCQ1,YCQ1,XCQ2,YCQ2,XCQ3,YCQ3)
C     FUNCTION ITROVR (XCA1,YCA1,XCA2,YCA2,XCA3,YCA3,
C    +                 XCB1,YCB1,XCB2,YCB2,XCB3,YCB3)
C
C This function has a non-zero value if and only if the 2D triangles
C defined by its arguments actually overlap.  (If they just touch each
C other, this version of ITROVR reports that they don't overlap.)
C
C Declare an arithmetic statement function that has one sign if point
C 3 is to the left of the line from point 1 to point 2 and a different
C sign if point 3 is to the right of that line.
C
        SIDE(X1,Y1,X2,Y2,X3,Y3)=(X1-X3)*(Y2-Y3)-(Y1-Y3)*(X2-X3)
C
        XCA1=.999*XCP1+.001*(XCP1+XCP2+XCP3)/3.
        YCA1=.999*YCP1+.001*(YCP1+YCP2+YCP3)/3.
        XCA2=.999*XCP2+.001*(XCP1+XCP2+XCP3)/3.
        YCA2=.999*YCP2+.001*(YCP1+YCP2+YCP3)/3.
        XCA3=.999*XCP3+.001*(XCP1+XCP2+XCP3)/3.
        YCA3=.999*YCP3+.001*(YCP1+YCP2+YCP3)/3.
        XCB1=.999*XCQ1+.001*(XCQ1+XCQ2+XCQ3)/3.
        YCB1=.999*YCQ1+.001*(YCQ1+YCQ2+YCQ3)/3.
        XCB2=.999*XCQ2+.001*(XCQ1+XCQ2+XCQ3)/3.
        YCB2=.999*YCQ2+.001*(YCQ1+YCQ2+YCQ3)/3.
        XCB3=.999*XCQ3+.001*(XCQ1+XCQ2+XCQ3)/3.
        YCB3=.999*YCQ3+.001*(YCQ1+YCQ2+YCQ3)/3.
C
C Initialize the value of the function to say that the triangles don't
C overlap.
C
        ITROVR=0
C
C If the X extents of the triangles don't overlap, then the triangles
C don't either.
C
        IF (MAX(XCA1,XCA2,XCA3).LE.MIN(XCB1,XCB2,XCB3)) RETURN
        IF (MIN(XCA1,XCA2,XCA3).GE.MAX(XCB1,XCB2,XCB3)) RETURN
C
C If the Y extents of the triangles don't overlap, then the triangles
C don't either.
C
        IF (MAX(YCA1,YCA2,YCA3).LE.MIN(YCB1,YCB2,YCB3)) RETURN
        IF (MIN(YCA1,YCA2,YCA3).GE.MAX(YCB1,YCB2,YCB3)) RETURN
C
C Re-initialize the value of the function to say that the triangles
C do overlap.
C
        ITROVR=1
C
C If any vertex of triangle B is inside triangle A, then the triangles
C overlap.  (Observe that a point is inside a triangle if and only if
C it is on the same side of all three edges of the triangle.)
C
        TMP1=SIDE(XCA1,YCA1,XCA2,YCA2,XCB1,YCB1)
        TMP2=SIDE(XCA2,YCA2,XCA3,YCA3,XCB1,YCB1)
        TMP3=SIDE(XCA3,YCA3,XCA1,YCA1,XCB1,YCB1)
C
        IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.
     +      (TMP1.GT.0..AND.TMP2.GT.0..AND.TMP3.GT.0.)) RETURN
C
        TMP1=SIDE(XCA1,YCA1,XCA2,YCA2,XCB2,YCB2)
        TMP2=SIDE(XCA2,YCA2,XCA3,YCA3,XCB2,YCB2)
        TMP3=SIDE(XCA3,YCA3,XCA1,YCA1,XCB2,YCB2)
C
        IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.
     +      (TMP1.GT.0..AND.TMP2.GT.0..AND.TMP3.GT.0.)) RETURN
C
        TMP1=SIDE(XCA1,YCA1,XCA2,YCA2,XCB3,YCB3)
        TMP2=SIDE(XCA2,YCA2,XCA3,YCA3,XCB3,YCB3)
        TMP3=SIDE(XCA3,YCA3,XCA1,YCA1,XCB3,YCB3)
C
        IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.
     +      (TMP1.GT.0..AND.TMP2.GT.0..AND.TMP3.GT.0.)) RETURN
C
C Similarly, if any vertex of triangle A is inside triangle B, then the
C triangles overlap.
C
        TMP1=SIDE(XCB1,YCB1,XCB2,YCB2,XCA1,YCA1)
        TMP2=SIDE(XCB2,YCB2,XCB3,YCB3,XCA1,YCA1)
        TMP3=SIDE(XCB3,YCB3,XCB1,YCB1,XCA1,YCA1)
C
        IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.
     +      (TMP1.GT.0..AND.TMP2.GT.0..AND.TMP3.GT.0.)) RETURN
C
        TMP1=SIDE(XCB1,YCB1,XCB2,YCB2,XCA2,YCA2)
        TMP2=SIDE(XCB2,YCB2,XCB3,YCB3,XCA2,YCA2)
        TMP3=SIDE(XCB3,YCB3,XCB1,YCB1,XCA2,YCA2)
C
        IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.
     +      (TMP1.GT.0..AND.TMP2.GT.0..AND.TMP3.GT.0.)) RETURN
C
        TMP1=SIDE(XCB1,YCB1,XCB2,YCB2,XCA3,YCA3)
        TMP2=SIDE(XCB2,YCB2,XCB3,YCB3,XCA3,YCA3)
        TMP3=SIDE(XCB3,YCB3,XCB1,YCB1,XCA3,YCA3)
C
        IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.
     +      (TMP1.GT.0..AND.TMP2.GT.0..AND.TMP3.GT.0.)) RETURN
C
C If the center point of either triangle is inside the other, then the
C triangles overlap.  (This takes care of a case in which one triangle
C is inside the other, but each of its vertices is on a side of the
C outer triangle.)
C
        TMP1=SIDE(XCA1,YCA1,XCA2,YCA2,(XCB1+XCB2+XCB3)/3.,
     +                                (YCB1+YCB2+YCB3)/3.)
        TMP2=SIDE(XCA2,YCA2,XCA3,YCA3,(XCB1+XCB2+XCB3)/3.,
     +                                (YCB1+YCB2+YCB3)/3.)
        TMP3=SIDE(XCA3,YCA3,XCA1,YCA1,(XCB1+XCB2+XCB3)/3.,
     +                                (YCB1+YCB2+YCB3)/3.)
C
        IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.
     +      (TMP1.GT.0..AND.TMP2.GT.0..AND.TMP3.GT.0.)) RETURN
C
        TMP1=SIDE(XCB1,YCB1,XCB2,YCB2,(XCA1+XCA2+XCA3)/3.,
     +                                (YCA1+YCA2+YCA3)/3.)
        TMP2=SIDE(XCB2,YCB2,XCB3,YCB3,(XCA1+XCA2+XCA3)/3.,
     +                                (YCA1+YCA2+YCA3)/3.)
        TMP3=SIDE(XCB3,YCB3,XCB1,YCB1,(XCA1+XCA2+XCA3)/3.,
     +                                (YCA1+YCA2+YCA3)/3.)
C
        IF ((TMP1.LT.0..AND.TMP2.LT.0..AND.TMP3.LT.0.).OR.
     +      (TMP1.GT.0..AND.TMP2.GT.0..AND.TMP3.GT.0.)) RETURN
C
C At this point, we have eliminated many possible cases.  A careful
C examination of the remaining cases indicates that it is sufficient
C to check for intersections of the edges of A with the edges of B.
C If any such intersection is found, the triangles overlap.
C
         IF (ILSINT (XCA1,YCA1,XCA2,YCA2,XCB1,YCB1,XCB2,YCB2).NE.0)
     +                                                       RETURN
         IF (ILSINT (XCA1,YCA1,XCA2,YCA2,XCB2,YCB2,XCB3,YCB3).NE.0)
     +                                                       RETURN
         IF (ILSINT (XCA1,YCA1,XCA2,YCA2,XCB3,YCB3,XCB1,YCB1).NE.0)
     +                                                       RETURN
         IF (ILSINT (XCA2,YCA2,XCA3,YCA3,XCB1,YCB1,XCB2,YCB2).NE.0)
     +                                                       RETURN
         IF (ILSINT (XCA2,YCA2,XCA3,YCA3,XCB2,YCB2,XCB3,YCB3).NE.0)
     +                                                       RETURN
         IF (ILSINT (XCA2,YCA2,XCA3,YCA3,XCB3,YCB3,XCB1,YCB1).NE.0)
     +                                                       RETURN
         IF (ILSINT (XCA3,YCA3,XCA1,YCA1,XCB1,YCB1,XCB2,YCB2).NE.0)
     +                                                       RETURN
         IF (ILSINT (XCA3,YCA3,XCA1,YCA1,XCB2,YCB2,XCB3,YCB3).NE.0)
     +                                                       RETURN
         IF (ILSINT (XCA3,YCA3,XCA1,YCA1,XCB3,YCB3,XCB1,YCB1).NE.0)
     +                                                       RETURN
C
C No overlap is possible; set the function value appropriately to
C indicate this.
C
        ITROVR=0
C
C Done.
C
        RETURN
C
      END
