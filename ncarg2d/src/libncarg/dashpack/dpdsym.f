C
C $Id: dpdsym.f,v 1.4 2008-07-27 00:16:58 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPDSYM (XPSW,YPSW,CSYM,WOSW,ANGD)
C
        CHARACTER*1 CSYM
C
C This routine is called by DPDRAW to draw a symbol at the point
C (XPSW,YPSW) in the world coordinate system.  The symbol to be
C drawn is the one associated with the character CSYM.  Its width
C is to be WOSW, in the world coordinate system, and it is to be
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
        IF (CSYM.EQ.'0'.OR.CSYM.EQ.'5') THEN
C
C The mark is a circle.
C
          DO 101 I=1,37
            ANGR=DTOR*(REAL(I-1)*10.)
            XCRA(I)=XPSW+(WOSW/2.)*COS(ANGR)
            YCRA(I)=YPSW+(WOSW/2.)*SIN(ANGR)
  101     CONTINUE
C
          NCRA=37
C
        ELSE IF (CSYM.EQ.'1'.OR.CSYM.EQ.'6') THEN
C
C The mark is a square.
C
          DO 102 I=1,5
            ANGR=DTOR*(ANGD+45.+REAL(I-1)*90.)
            XCRA(I)=XPSW+1.25331*(WOSW/2.)*COS(ANGR)
            YCRA(I)=YPSW+1.25331*(WOSW/2.)*SIN(ANGR)
  102     CONTINUE
C
          NCRA=5
C
        ELSE IF (CSYM.EQ.'2'.OR.CSYM.EQ.'7') THEN
C
C The mark is a triangle.
C
          DO 103 I=1,4
            ANGR=DTOR*(ANGD+90.+REAL(I-1)*120.)
            XCRA(I)=XPSW+1.55512*(WOSW/2.)*COS(ANGR)
            YCRA(I)=YPSW+1.55512*(WOSW/2.)*SIN(ANGR)
  103     CONTINUE
C
          NCRA=4
C
        ELSE IF (CSYM.EQ.'3'.OR.CSYM.EQ.'8') THEN
C
C The mark is a diamond.
C
          DO 104 I=1,5
            ANGR=DTOR*(ANGD+REAL(I-1)*90.)
            XCRA(I)=XPSW+1.25331*(WOSW/2.)*COS(ANGR)
            YCRA(I)=YPSW+1.25331*(WOSW/2.)*SIN(ANGR)
  104     CONTINUE
C
          NCRA=5
C
        ELSE IF (CSYM.EQ.'4'.OR.CSYM.EQ.'9') THEN
C
C The mark is a star.
C
          DO 105 I=1,11
            ANGR=DTOR*(ANGD+18.+REAL(I-1)*36.)
            IF (MOD(I,2).EQ.0) THEN
              RADI=.381966
            ELSE
              RADI=1.
            END IF
            XCRA(I)=XPSW+1.67289*RADI*(WOSW/2.)*COS(ANGR)
            YCRA(I)=YPSW+1.67289*RADI*(WOSW/2.)*SIN(ANGR)
  105     CONTINUE
C
          NCRA=11

        END IF
C
C Fill or outline the mark.
C
        IF (CSYM.GE.'0'.AND.CSYM.LE.'4') THEN
          CALL GFA (NCRA,XCRA,YCRA)
        ELSE IF (CSYM.GE.'5'.AND.CSYM.LE.'9') THEN
          CALL GPL (NCRA,XCRA,YCRA)
        END IF
C
C Done.
C
        RETURN
C
      END
