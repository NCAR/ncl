C
C $Id: tdotri.f,v 1.4 2008-07-27 00:17:33 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDOTRI (RTRI,MTRI,NTRI,RTWK,ITWK,IORD)
C
        PARAMETER (EPSI=.0002,OMEP=1.-EPSI)
C
        DIMENSION RTRI(10,MTRI),RTWK(MTRI,2),ITWK(MTRI)
C
C This routine, given a list of NTRI triangles in the array RTRI and
C a real scratch array RTWK of length at least MTRI x 2 , determines
C the order in which those triangles ought to be rendered and returns
C a permutation of the integers from 1 to NTRI in the array ITWK to
C define that permutation.  Note that the value of NTRI may increase
C as a result of calling TDOTRI; it is sometimes necessary to break
C one or more triangles into pieces and then re-insert those pieces
C in the triangle list.  Arguments of TDOTRI are as follows:
C
C   RTRI is a real array containing a list of triangles; it is
C   dimensioned 10 x MTRI.  Each 10-word entry in the list consists
C   of the U, V, and W coordinates of each of three points, followed
C   by a rendering style index.
C
C   MTRI is an input integer specifying the second dimension of the
C   array RTRI: the maximum number of triangles the list will hold.
C
C   NTRI is an input/output integer specifying the number of triangles
C   currently in the array RTRI.
C
C   RTWK is a real workspace array, dimensioned MTRI x 2.
C
C   ITWK is an integer output array in which a sorted permutation of
C   the integers from 1 to NTRI is returned.
C
C   IORD is a flag that says how the triangles are to be ordered.  The
C   value 0 implies ordering by decreasing distance of the triangle
C   midpoints from the eye, -1 implies ordering by decreasing distance
C   of the triangle farpoints from the eye, and +1 implies ordering by
C   decreasing distance of the triangle farpoints from the eye, with
C   adjustments made by running an algorithm from Foley and Van Dam.
C
C Declare required TDPACK common blocks.
C
        COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
        COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
        COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
        SAVE   /TDCOM1/
C
C The following arithmetic statement functions are used to see whether
C a given point is on one side or the other of the planes defined by
C the 3-space triangles P and Q.
C
        PLNP(X,Y,Z)=ACPP*X+BCPP*Y+CCPP*Z+DCPP
        PLNQ(X,Y,Z)=ACPQ*X+BCPQ*Y+CCPQ*Z+DCPQ
C
C Create the initial ordering of the triangles by sorting on either the
C squares of the distances of the midpoints of the triangles or on the
C squares of the distances of the farpoints of the triangles (from the
C observer's eye), as implied by the value of IORD.  The order of the
C triangles is not actually changed in the triangle list; instead, a
C permutation of the integers from 1 to NTRI is returned in ITWK, such
C that RTWK(ITWK(1),1) <= RTWK(ITWK(2),1) <= RTWK(ITWK(3),1) <= ...
C RTWK(ITWK(NTRI),1).
C
        IF (IORD.EQ.0) THEN
C
          DO 101 I=1,NTRI
            RTWK(I,1)=(XE-((RTRI(1,I)+
     +                      RTRI(4,I)+
     +                      RTRI(7,I))/3.))**2+
     +                (YE-((RTRI(2,I)+
     +                      RTRI(5,I)+
     +                      RTRI(8,I))/3.))**2+
     +                (ZE-((RTRI(3,I)+
     +                      RTRI(6,I)+
     +                      RTRI(9,I))/3.))**2
  101     CONTINUE
C
        ELSE
C
          DO 102 I=1,NTRI
            DSQ1=(XE-RTRI(1,I))**2+(YE-RTRI(2,I))**2+(ZE-RTRI(3,I))**2
            DSQ2=(XE-RTRI(4,I))**2+(YE-RTRI(5,I))**2+(ZE-RTRI(6,I))**2
            DSQ3=(XE-RTRI(7,I))**2+(YE-RTRI(8,I))**2+(ZE-RTRI(9,I))**2
            RTWK(I,1)=MAX(DSQ1,DSQ2,DSQ3)
            RTWK(I,2)=MIN(DSQ1,DSQ2,DSQ3)
  102     CONTINUE
C
        END IF
C
        CALL TDSORT (RTWK,NTRI,1,ITWK)
C
C Quit if the ordering flag says not to check for ordering problems in
C the list.  Otherwise, the algorithm from Foley and Van Dam will be
C used to check for such problems.
C
        IF (IORD.LE.0) RETURN
C
C Compute a tolerance to use below.
C
        ETST=EPSI*(WR-WL)
C
C Multiply the rendering style selectors by 10 to provide room for an
C array of loop-detection flags required by the algorithm.
C
        DO 103 I=1,NTRI
          RTRI(10,I)=REAL(10*INT(ABS(RTRI(10,I))))
  103   CONTINUE
C
C Look through the triangle list for pairs of triangles whose order is
C not the order in which the triangles ought to be rendered.  The index
C ITRP steps through the entire list of triangles, save the last.
C
        ITRP=0
C
  104   ITRP=ITRP+1
C
        IF (ITRP.LT.NTRI) THEN
C
          ITWP=ITWK(ITRP)
C
C Get the X and Y coordinates of the vertices of the projection of
C triangle P in the projection plane.
C
          CALL TDPRPT (RTRI(1,ITWP),RTRI(2,ITWP),RTRI(3,ITWP),
     +                                              XCP1,YCP1)
          CALL TDPRPT (RTRI(4,ITWP),RTRI(5,ITWP),RTRI(6,ITWP),
     +                                              XCP2,YCP2)
          CALL TDPRPT (RTRI(7,ITWP),RTRI(8,ITWP),RTRI(9,ITWP),
     +                                              XCP3,YCP3)
C
C The index ITRQ steps from the position of ITRP through all the
C triangles whose range of distances from the viewpoint indicates
C a possible overlap of the projections of the triangles P and Q.
C
          ITRQ=ITRP
C
  105     ITRQ=ITRQ+1
C
          IF (ITRQ.LE.NTRI) THEN
C
            ITWQ=ITWK(ITRQ)
C
            IF (RTWK(ITWQ,1).LE.RTWK(ITWP,2)) THEN
C
              IF (MOD(INT(RTRI(10,ITWQ)),10).NE.0) GO TO 105
C
              GO TO 104
C
            ELSE
C
C Get the X and Y coordinates of the vertices of the projection of
C triangle Q in the projection plane.
C
              CALL TDPRPT (RTRI(1,ITWQ),RTRI(2,ITWQ),RTRI(3,ITWQ),
     +                                                  XCQ1,YCQ1)
              CALL TDPRPT (RTRI(4,ITWQ),RTRI(5,ITWQ),RTRI(6,ITWQ),
     +                                                  XCQ2,YCQ2)
              CALL TDPRPT (RTRI(7,ITWQ),RTRI(8,ITWQ),RTRI(9,ITWQ),
     +                                                  XCQ3,YCQ3)
C
C Jump if the X-coordinate ranges of the projected triangles don't
C overlap; such a pair of triangles can be rendered in either order.
C
              IF (MAX(XCP1,XCP2,XCP3).LE.MIN(XCQ1,XCQ2,XCQ3).OR.
     +            MIN(XCP1,XCP2,XCP3).GE.MAX(XCQ1,XCQ2,XCQ3)) GO TO 105
C
C Similarly, jump if the Y-coordinate ranges of the projected triangles
C don't overlap.
C
              IF (MAX(YCP1,YCP2,YCP3).LE.MIN(YCQ1,YCQ2,YCQ3).OR.
     +            MIN(YCP1,YCP2,YCP3).GE.MAX(YCQ1,YCQ2,YCQ3)) GO TO 105
C
C Compute the coefficients A, B, C, and D in the equation defining the
C plane of triangle Q.
C
              ACPQ=(RTRI(5,ITWQ)-RTRI(2,ITWQ))*
     +             (RTRI(9,ITWQ)-RTRI(3,ITWQ))-
     +             (RTRI(8,ITWQ)-RTRI(2,ITWQ))*
     +             (RTRI(6,ITWQ)-RTRI(3,ITWQ))
C
              BCPQ=(RTRI(6,ITWQ)-RTRI(3,ITWQ))*
     +             (RTRI(7,ITWQ)-RTRI(1,ITWQ))-
     +             (RTRI(9,ITWQ)-RTRI(3,ITWQ))*
     +             (RTRI(4,ITWQ)-RTRI(1,ITWQ))
C
              CCPQ=(RTRI(4,ITWQ)-RTRI(1,ITWQ))*
     +             (RTRI(8,ITWQ)-RTRI(2,ITWQ))-
     +             (RTRI(7,ITWQ)-RTRI(1,ITWQ))*
     +             (RTRI(5,ITWQ)-RTRI(2,ITWQ))
C
              DCPQ=-(ACPQ*RTRI(1,ITWQ)+
     +               BCPQ*RTRI(2,ITWQ)+
     +               CCPQ*RTRI(3,ITWQ))
C
C Compute quantities proportional to the directed distances from the
C plane of triangle Q to the viewpoint (the eye) and to each vertex
C of the triangle P.  ("PQVP" stands for "Position, relative to Q's
C plane, of ViewPoint" and "PQPn" stands for "Position, relative to
C Q's plane, of point Pn".)
C
              PQVP=PLNQ(XE,YE,ZE)
C
              PQP1=PLNQ(RTRI(1,ITWP),RTRI(2,ITWP),RTRI(3,ITWP))
              PQP2=PLNQ(RTRI(4,ITWP),RTRI(5,ITWP),RTRI(6,ITWP))
              PQP3=PLNQ(RTRI(7,ITWP),RTRI(8,ITWP),RTRI(9,ITWP))
C
              PMIN=MIN(PQP1,PQP2,PQP3)
              PMAX=MAX(PQP1,PQP2,PQP3)
C
              PTST=SQRT(ACPQ*ACPQ+BCPQ*BCPQ+CCPQ*CCPQ)*ETST
C
C If triangle P is on the opposite side of triangle Q's plane from the
C viewpoint, then P cannot obscure Q, so jump.
C
              IF ((PQVP.LE.0..AND.PMIN.GE.-PTST).OR.
     +            (PQVP.GE.0..AND.PMAX.LE. PTST)    ) GO TO 105
C
C Compute the coefficients A, B, C, and D in the equation defining the
C plane of triangle P.
C
              ACPP=(RTRI(5,ITWP)-RTRI(2,ITWP))*
     +             (RTRI(9,ITWP)-RTRI(3,ITWP))-
     +             (RTRI(8,ITWP)-RTRI(2,ITWP))*
     +             (RTRI(6,ITWP)-RTRI(3,ITWP))
C
              BCPP=(RTRI(6,ITWP)-RTRI(3,ITWP))*
     +             (RTRI(7,ITWP)-RTRI(1,ITWP))-
     +             (RTRI(9,ITWP)-RTRI(3,ITWP))*
     +             (RTRI(4,ITWP)-RTRI(1,ITWP))
C
              CCPP=(RTRI(4,ITWP)-RTRI(1,ITWP))*
     +             (RTRI(8,ITWP)-RTRI(2,ITWP))-
     +             (RTRI(7,ITWP)-RTRI(1,ITWP))*
     +             (RTRI(5,ITWP)-RTRI(2,ITWP))
C
              DCPP=-(ACPP*RTRI(1,ITWP)+
     +               BCPP*RTRI(2,ITWP)+
     +               CCPP*RTRI(3,ITWP))
C
C Compute quantities proportional to the directed distances from the
C plane of triangle P to the viewpoint (the eye) and to each vertex
C of the triangle Q.  ("PPVP" stands for "Position, relative to P's
C plane, of ViewPoint" and "PPQn" stands for "Position, relative to
C P's plane, of point Qn".)
C
              PPVP=PLNP(XE,YE,ZE)
C
              PPQ1=PLNP(RTRI(1,ITWQ),RTRI(2,ITWQ),RTRI(3,ITWQ))
              PPQ2=PLNP(RTRI(4,ITWQ),RTRI(5,ITWQ),RTRI(6,ITWQ))
              PPQ3=PLNP(RTRI(7,ITWQ),RTRI(8,ITWQ),RTRI(9,ITWQ))
C
              QMIN=MIN(PPQ1,PPQ2,PPQ3)
              QMAX=MAX(PPQ1,PPQ2,PPQ3)
C
              QTST=SQRT(ACPP*ACPP+BCPP*BCPP+CCPP*CCPP)*ETST
C
C If triangle Q is on the same side of triangle P's plane from the
C viewpoint, then P cannot obscure Q, so jump.
C
              IF ((PPVP.LE.0..AND.QMAX.LE. QTST).OR.
     +            (PPVP.GE.0..AND.QMIN.GE.-QTST)    ) GO TO 105
C
C If the projections of P and Q in the projection plane don't overlap
C at all, then P cannot obscure Q, so jump.
C
              IF (ITROVR(XCP1,YCP1,XCP2,YCP2,XCP3,YCP3,
     +                   XCQ1,YCQ1,XCQ2,YCQ2,XCQ3,YCQ3).EQ.0) GO TO 105
C
C Otherwise, the situation is in doubt: we have not established that P
C cannot obscure Q.  Unless Q has previously been moved to the start
C of the list (indicating an "infinite" loop), check for the possibility
C that it cannot obscure P; if so, move Q to precede P in the list and
C make it the new P.
C
              IF (MOD(INT(RTRI(10,ITWQ)),10).EQ.0) THEN
                IF ((PPVP.LE.0..AND.QMIN.GE.-QTST).OR.
     +              (PPVP.GE.0..AND.QMAX.LE. QTST).OR.
     +              (PQVP.LE.0..AND.PMAX.LE. PTST).OR.
     +              (PQVP.GE.0..AND.PMIN.GE.-PTST)    ) THEN
                  DO 106 I=ITRQ,ITRP+1,-1
                    ITWK(I)=ITWK(I-1)
  106             CONTINUE
                  ITWK(ITRP)=ITWQ
                  RTRI(10,ITWQ)=RTRI(10,ITWQ)+1.
                  ITRP=ITRP-1
                  GO TO 104
                END IF
              END IF
C
C Otherwise, the situation is still in doubt.  There is no satisfactory
C order in which to process P and Q, so we split either P or Q into
C three pieces, put those pieces back in the proper positions in the
C triangle list, and resume the scan.  First, try to split Q.
C
              IF (QMIN.LT.0..AND.QMAX.GT.0.) THEN
                IF      (SIGN(1.,PPQ1).EQ.SIGN(1.,PPQ2)) THEN
                  ITQ1=1
                  ITQ2=4
                  ITQ3=7
                  TATA=ABS(PPQ3)/(ABS(PPQ3)+ABS(PPQ1))
                  TATB=ABS(PPQ2)/(ABS(PPQ2)+ABS(PPQ3))
                ELSE IF (SIGN(1.,PPQ2).EQ.SIGN(1.,PPQ3)) THEN
                  ITQ1=4
                  ITQ2=7
                  ITQ3=1
                  TATA=ABS(PPQ1)/(ABS(PPQ1)+ABS(PPQ2))
                  TATB=ABS(PPQ3)/(ABS(PPQ3)+ABS(PPQ1))
                ELSE
                  ITQ1=7
                  ITQ2=1
                  ITQ3=4
                  TATA=ABS(PPQ2)/(ABS(PPQ2)+ABS(PPQ3))
                  TATB=ABS(PPQ1)/(ABS(PPQ1)+ABS(PPQ2))
                END IF
              ELSE
                GO TO 110
              END IF
C
              IF (TATA.LE.EPSI.OR.TATB.GE.OMEP.OR.
     +           (TATA.GE.OMEP.AND.TATB.LE.EPSI)) GO TO 110
C
              XCPA=(1.-TATA)*RTRI(ITQ3  ,ITWQ)+TATA*RTRI(ITQ1  ,ITWQ)
              YCPA=(1.-TATA)*RTRI(ITQ3+1,ITWQ)+TATA*RTRI(ITQ1+1,ITWQ)
              ZCPA=(1.-TATA)*RTRI(ITQ3+2,ITWQ)+TATA*RTRI(ITQ1+2,ITWQ)
C
              XCPB=(1.-TATB)*RTRI(ITQ2  ,ITWQ)+TATB*RTRI(ITQ3  ,ITWQ)
              YCPB=(1.-TATB)*RTRI(ITQ2+1,ITWQ)+TATB*RTRI(ITQ3+1,ITWQ)
              ZCPB=(1.-TATB)*RTRI(ITQ2+2,ITWQ)+TATB*RTRI(ITQ3+2,ITWQ)
C
              NTRS=NTRI
C
              IF (TATA.GE.OMEP.OR.TATB.LE.EPSI) THEN
                IF (NTRI.GE.MTRI) GO TO 116
                NTRI=NTRI+1
                IF (TATB.LE.EPSI) THEN
                  RTRI(1,NTRI)=RTRI(ITQ1  ,ITWQ)
                  RTRI(2,NTRI)=RTRI(ITQ1+1,ITWQ)
                  RTRI(3,NTRI)=RTRI(ITQ1+2,ITWQ)
                  RTRI(4,NTRI)=RTRI(ITQ2  ,ITWQ)
                  RTRI(5,NTRI)=RTRI(ITQ2+1,ITWQ)
                  RTRI(6,NTRI)=RTRI(ITQ2+2,ITWQ)
                  RTRI(7,NTRI)=XCPA
                  RTRI(8,NTRI)=YCPA
                  RTRI(9,NTRI)=ZCPA
                  RTRI(ITQ1  ,ITWQ)=XCPA
                  RTRI(ITQ1+1,ITWQ)=YCPA
                  RTRI(ITQ1+2,ITWQ)=ZCPA
                ELSE
                  RTRI(1,NTRI)=RTRI(ITQ1  ,ITWQ)
                  RTRI(2,NTRI)=RTRI(ITQ1+1,ITWQ)
                  RTRI(3,NTRI)=RTRI(ITQ1+2,ITWQ)
                  RTRI(4,NTRI)=RTRI(ITQ2  ,ITWQ)
                  RTRI(5,NTRI)=RTRI(ITQ2+1,ITWQ)
                  RTRI(6,NTRI)=RTRI(ITQ2+2,ITWQ)
                  RTRI(7,NTRI)=XCPB
                  RTRI(8,NTRI)=YCPB
                  RTRI(9,NTRI)=ZCPB
                  RTRI(ITQ2  ,ITWQ)=XCPB
                  RTRI(ITQ2+1,ITWQ)=YCPB
                  RTRI(ITQ2+2,ITWQ)=ZCPB
                END IF
                RTRI(10,NTRI)=REAL(10*(INT(RTRI(10,ITWQ))/10))
                ITWK(NTRI)=NTRI
              ELSE
                IF (NTRI.GE.MTRI) GO TO 116
                NTRI=NTRI+1
                RTRI(1,NTRI)=RTRI(ITQ1  ,ITWQ)
                RTRI(2,NTRI)=RTRI(ITQ1+1,ITWQ)
                RTRI(3,NTRI)=RTRI(ITQ1+2,ITWQ)
                RTRI(4,NTRI)=RTRI(ITQ2  ,ITWQ)
                RTRI(5,NTRI)=RTRI(ITQ2+1,ITWQ)
                RTRI(6,NTRI)=RTRI(ITQ2+2,ITWQ)
                RTRI(7,NTRI)=XCPA
                RTRI(8,NTRI)=YCPA
                RTRI(9,NTRI)=ZCPA
                RTRI(10,NTRI)=REAL(10*(INT(RTRI(10,ITWQ))/10))
                ITWK(NTRI)=NTRI
                IF (NTRI.GE.MTRI) GO TO 116
                NTRI=NTRI+1
                RTRI(1,NTRI)=RTRI(ITQ2  ,ITWQ)
                RTRI(2,NTRI)=RTRI(ITQ2+1,ITWQ)
                RTRI(3,NTRI)=RTRI(ITQ2+2,ITWQ)
                RTRI(4,NTRI)=XCPB
                RTRI(5,NTRI)=YCPB
                RTRI(6,NTRI)=ZCPB
                RTRI(7,NTRI)=XCPA
                RTRI(8,NTRI)=YCPA
                RTRI(9,NTRI)=ZCPA
                RTRI(10,NTRI)=REAL(10*(INT(RTRI(10,ITWQ))/10))
                ITWK(NTRI)=NTRI
                RTRI(ITQ1  ,ITWQ)=XCPA
                RTRI(ITQ1+1,ITWQ)=YCPA
                RTRI(ITQ1+2,ITWQ)=ZCPA
                RTRI(ITQ2  ,ITWQ)=XCPB
                RTRI(ITQ2+1,ITWQ)=YCPB
                RTRI(ITQ2+2,ITWQ)=ZCPB
              END IF
C
              DSQ1=(XE-RTRI(1,ITWQ))**2+
     +             (YE-RTRI(2,ITWQ))**2+
     +             (ZE-RTRI(3,ITWQ))**2
              DSQ2=(XE-RTRI(4,ITWQ))**2+
     +             (YE-RTRI(5,ITWQ))**2+
     +             (ZE-RTRI(6,ITWQ))**2
              DSQ3=(XE-RTRI(7,ITWQ))**2+
     +             (YE-RTRI(8,ITWQ))**2+
     +             (ZE-RTRI(9,ITWQ))**2
              RTWK(ITWQ,1)=MAX(DSQ1,DSQ2,DSQ3)
              RTWK(ITWQ,2)=MIN(DSQ1,DSQ2,DSQ3)
C
              RTRI(10,ITWQ)=REAL(10*(INT(RTRI(10,ITWQ))/10))
C
              ITRT=ITRQ
C
  107         IF (ITRT.LT.NTRS) THEN
                IF (MOD(INT(RTRI(10,ITWK(ITRT+1))),10).NE.0.OR.
     +              RTWK(ITWQ,1).LT.RTWK(ITWK(ITRT+1),1)) THEN
                  ITWK(ITRT)=ITWK(ITRT+1)
                  ITWK(ITRT+1)=ITWQ
                  ITRT=ITRT+1
                  GO TO 107
                END IF
              END IF
C
              DO 109 I=NTRS+1,NTRI
                DSQ1=(XE-RTRI(1,I))**2+
     +               (YE-RTRI(2,I))**2+
     +               (ZE-RTRI(3,I))**2
                DSQ2=(XE-RTRI(4,I))**2+
     +               (YE-RTRI(5,I))**2+
     +               (ZE-RTRI(6,I))**2
                DSQ3=(XE-RTRI(7,I))**2+
     +               (YE-RTRI(8,I))**2+
     +               (ZE-RTRI(9,I))**2
                RTWK(I,1)=MAX(DSQ1,DSQ2,DSQ3)
                RTWK(I,2)=MIN(DSQ1,DSQ2,DSQ3)
                ITRT=I
  108           IF (ITRT.GT.ITRP+1) THEN
                  IF (MOD(INT(RTRI(10,ITWK(ITRT-1))),10).EQ.0.AND.
     +                RTWK(I,1).GT.RTWK(ITWK(ITRT-1),1)) THEN
                    ITWK(ITRT)=ITWK(ITRT-1)
                    ITWK(ITRT-1)=I
                    ITRT=ITRT-1
                    GO TO 108
                  END IF
                END IF
  109         CONTINUE
C
              ITRQ=ITRQ-1
C
              GO TO 105
C
C For one reason or another, Q could not be split.  Try to split P.
C
  110         IF (PMIN.LT.0..AND.PMAX.GT.0.) THEN
                IF      (SIGN(1.,PQP1).EQ.SIGN(1.,PQP2)) THEN
                  ITP1=1
                  ITP2=4
                  ITP3=7
                  TATA=ABS(PQP3)/(ABS(PQP3)+ABS(PQP1))
                  TATB=ABS(PQP2)/(ABS(PQP2)+ABS(PQP3))
                ELSE IF (SIGN(1.,PQP2).EQ.SIGN(1.,PQP3)) THEN
                  ITP1=4
                  ITP2=7
                  ITP3=1
                  TATA=ABS(PQP1)/(ABS(PQP1)+ABS(PQP2))
                  TATB=ABS(PQP3)/(ABS(PQP3)+ABS(PQP1))
                ELSE
                  ITP1=7
                  ITP2=1
                  ITP3=4
                  TATA=ABS(PQP2)/(ABS(PQP2)+ABS(PQP3))
                  TATB=ABS(PQP1)/(ABS(PQP1)+ABS(PQP2))
                END IF
              ELSE
                GO TO 114
              END IF
C
              IF (TATA.LE.EPSI.OR.TATB.GE.OMEP.OR.
     +           (TATA.GE.OMEP.AND.TATB.LE.EPSI)) GO TO 114
C
              XCPA=(1.-TATA)*RTRI(ITP3  ,ITWP)+TATA*RTRI(ITP1  ,ITWP)
              YCPA=(1.-TATA)*RTRI(ITP3+1,ITWP)+TATA*RTRI(ITP1+1,ITWP)
              ZCPA=(1.-TATA)*RTRI(ITP3+2,ITWP)+TATA*RTRI(ITP1+2,ITWP)
C
              XCPB=(1.-TATB)*RTRI(ITP2  ,ITWP)+TATB*RTRI(ITP3  ,ITWP)
              YCPB=(1.-TATB)*RTRI(ITP2+1,ITWP)+TATB*RTRI(ITP3+1,ITWP)
              ZCPB=(1.-TATB)*RTRI(ITP2+2,ITWP)+TATB*RTRI(ITP3+2,ITWP)
C
              NTRS=NTRI
C
              IF (TATA.GE.OMEP.OR.TATB.LE.EPSI) THEN
                IF (NTRI.GE.MTRI) GO TO 116
                NTRI=NTRI+1
                IF (TATB.LE.EPSI) THEN
                  RTRI(1,NTRI)=RTRI(ITP1  ,ITWP)
                  RTRI(2,NTRI)=RTRI(ITP1+1,ITWP)
                  RTRI(3,NTRI)=RTRI(ITP1+2,ITWP)
                  RTRI(4,NTRI)=RTRI(ITP2  ,ITWP)
                  RTRI(5,NTRI)=RTRI(ITP2+1,ITWP)
                  RTRI(6,NTRI)=RTRI(ITP2+2,ITWP)
                  RTRI(7,NTRI)=XCPA
                  RTRI(8,NTRI)=YCPA
                  RTRI(9,NTRI)=ZCPA
                  RTRI(ITP1  ,ITWP)=XCPA
                  RTRI(ITP1+1,ITWP)=YCPA
                  RTRI(ITP1+2,ITWP)=ZCPA
                ELSE
                  RTRI(1,NTRI)=RTRI(ITP1  ,ITWP)
                  RTRI(2,NTRI)=RTRI(ITP1+1,ITWP)
                  RTRI(3,NTRI)=RTRI(ITP1+2,ITWP)
                  RTRI(4,NTRI)=RTRI(ITP2  ,ITWP)
                  RTRI(5,NTRI)=RTRI(ITP2+1,ITWP)
                  RTRI(6,NTRI)=RTRI(ITP2+2,ITWP)
                  RTRI(7,NTRI)=XCPB
                  RTRI(8,NTRI)=YCPB
                  RTRI(9,NTRI)=ZCPB
                  RTRI(ITP2  ,ITWP)=XCPB
                  RTRI(ITP2+1,ITWP)=YCPB
                  RTRI(ITP2+2,ITWP)=ZCPB
                END IF
                RTRI(10,NTRI)=REAL(10*(INT(RTRI(10,ITWP))/10))
                ITWK(NTRI)=NTRI
              ELSE
                IF (NTRI.GE.MTRI) GO TO 116
                NTRI=NTRI+1
                RTRI(1,NTRI)=RTRI(ITP1  ,ITWP)
                RTRI(2,NTRI)=RTRI(ITP1+1,ITWP)
                RTRI(3,NTRI)=RTRI(ITP1+2,ITWP)
                RTRI(4,NTRI)=RTRI(ITP2  ,ITWP)
                RTRI(5,NTRI)=RTRI(ITP2+1,ITWP)
                RTRI(6,NTRI)=RTRI(ITP2+2,ITWP)
                RTRI(7,NTRI)=XCPA
                RTRI(8,NTRI)=YCPA
                RTRI(9,NTRI)=ZCPA
                RTRI(10,NTRI)=REAL(10*(INT(RTRI(10,ITWP))/10))
                ITWK(NTRI)=NTRI
                IF (NTRI.GE.MTRI) GO TO 116
                NTRI=NTRI+1
                RTRI(1,NTRI)=RTRI(ITP2  ,ITWP)
                RTRI(2,NTRI)=RTRI(ITP2+1,ITWP)
                RTRI(3,NTRI)=RTRI(ITP2+2,ITWP)
                RTRI(4,NTRI)=XCPB
                RTRI(5,NTRI)=YCPB
                RTRI(6,NTRI)=ZCPB
                RTRI(7,NTRI)=XCPA
                RTRI(8,NTRI)=YCPA
                RTRI(9,NTRI)=ZCPA
                RTRI(10,NTRI)=REAL(10*(INT(RTRI(10,ITWP))/10))
                ITWK(NTRI)=NTRI
                RTRI(ITP1  ,ITWP)=XCPA
                RTRI(ITP1+1,ITWP)=YCPA
                RTRI(ITP1+2,ITWP)=ZCPA
                RTRI(ITP2  ,ITWP)=XCPB
                RTRI(ITP2+1,ITWP)=YCPB
                RTRI(ITP2+2,ITWP)=ZCPB
              END IF
C
              DSQ1=(XE-RTRI(1,ITWP))**2+
     +             (YE-RTRI(2,ITWP))**2+
     +             (ZE-RTRI(3,ITWP))**2
              DSQ2=(XE-RTRI(4,ITWP))**2+
     +             (YE-RTRI(5,ITWP))**2+
     +             (ZE-RTRI(6,ITWP))**2
              DSQ3=(XE-RTRI(7,ITWP))**2+
     +             (YE-RTRI(8,ITWP))**2+
     +             (ZE-RTRI(9,ITWP))**2
              RTWK(ITWP,1)=MAX(DSQ1,DSQ2,DSQ3)
              RTWK(ITWP,2)=MIN(DSQ1,DSQ2,DSQ3)
C
              RTRI(10,ITWP)=REAL(10*(INT(RTRI(10,ITWP))/10))
C
              ITRT=ITRP
C
  111         IF (ITRT.LT.NTRS) THEN
                IF (MOD(INT(RTRI(10,ITWK(ITRT+1))),10).NE.0.OR.
     +              RTWK(ITWP,1).LT.RTWK(ITWK(ITRT+1),1)) THEN
                  ITWK(ITRT)=ITWK(ITRT+1)
                  ITWK(ITRT+1)=ITWP
                  ITRT=ITRT+1
                  GO TO 111
                END IF
              END IF
C
              DO 113 I=NTRS+1,NTRI
                DSQ1=(XE-RTRI(1,I))**2+
     +               (YE-RTRI(2,I))**2+
     +               (ZE-RTRI(3,I))**2
                DSQ2=(XE-RTRI(4,I))**2+
     +               (YE-RTRI(5,I))**2+
     +               (ZE-RTRI(6,I))**2
                DSQ3=(XE-RTRI(7,I))**2+
     +               (YE-RTRI(8,I))**2+
     +               (ZE-RTRI(9,I))**2
                RTWK(I,1)=MAX(DSQ1,DSQ2,DSQ3)
                RTWK(I,2)=MIN(DSQ1,DSQ2,DSQ3)
                ITRT=I
  112           IF (ITRT.GT.ITRP) THEN
                  IF (MOD(INT(RTRI(10,ITWK(ITRT-1))),10).EQ.0.AND.
     +                RTWK(I,1).GT.RTWK(ITWK(ITRT-1),1)) THEN
                    ITWK(ITRT)=ITWK(ITRT-1)
                    ITWK(ITRT-1)=I
                    ITRT=ITRT-1
                    GO TO 112
                  END IF
                END IF
  113         CONTINUE
C
              ITRP=ITRP-1
C
              GO TO 104
C
C If neither Q nor P can be split, move Q to precede P anyway, checking
C first for an infinite loop.
C
  114         IF (MOD(INT(RTRI(10,ITWQ)),10).GT.8) GO TO 105
C
              IF ((PPVP.LE.0..AND.QMIN.GE.-QTST).OR.
     +            (PPVP.GE.0..AND.QMAX.LE. QTST).OR.
     +            (PQVP.LE.0..AND.PMAX.LE. PTST).OR.
     +            (PQVP.GE.0..AND.PMIN.GE.-PTST)    ) THEN
C
                DO 115 I=ITRQ,ITRP+1,-1
                  ITWK(I)=ITWK(I-1)
  115           CONTINUE
C
                ITWK(ITRP)=ITWQ
C
                RTRI(10,ITWQ)=RTRI(10,ITWQ)+1.
C
                ITRP=ITRP-1
C
                GO TO 104
C
              ELSE
C
                GO TO 105
C
              END IF
C
            END IF
C
C           GO TO 105
C
          END IF
C
          GO TO 104
C
        END IF
C
C Force all the returned indices to be in the correct range.
C
  116   DO 117 I=1,NTRI
          RTRI(10,I)=REAL(INT(RTRI(10,I))/10)
  117   CONTINUE
C
C Done.
C
        RETURN
C
      END
