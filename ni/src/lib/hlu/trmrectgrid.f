C
C $Id: trmrectgrid.f,v 1.1 2004-03-11 02:00:34 dbrown Exp $
C
      SUBROUTINE TRMRGR (IDIM,JDIM,XARR,YARR,
     +                   RDAT,ISCR,SVAL,
     +                   RPNT,MPNT,NPNT,LOPN,
     +                   IEDG,MEDG,NEDG,LOEN,
     +                   ITRI,MTRI,NTRI,LOTN)
C
      DIMENSION XARR(IDIM,JDIM),YARR(IDIM,JDIM),RDAT(IDIM,JDIM)
      DIMENSION ISCR(IDIM,JDIM,4)
      DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI)
C
C Given arrays defining a rectangular mesh of data, TRMRGR returns a
C triangular mesh representing the data.
C
C The arguments are as follows:
C
C IDIM - an input expression of type INTEGER - the first dimension of
C the rectangular mesh.
C
C JDIM - an input expression of type INTEGER - the second dimension of
C the rectangular mesh.
C
C XARR - an input array of type REAL, dimensioned IDIM by JDIM - the
C values of X for the points of the rectangular mesh.
C
C YARR - an input array of type REAL, dimensioned IDIM by JDIM - the
C values of Y for the points of the rectangular mesh.
C
C RDAT - an input array of type REAL, dimensioned IDIM by JDIM - the
C values of the data field for the points of the rectangular mesh.
C
C ISCR - a scratch array of type INTEGER, dimensioned IDIM*JDIM*4.
C
C SVAL - an input expression of type REAL - a value which, if used in
C the array RDAT, marks that datum as "special" or "missing".
C
C RPNT is a one-dimensional output array of type REAL in which the list
C of the points of the triangular mesh is placed.
C
C MPNT is an input expression of type INTEGER specifying the length of
C RPNT.
C
C NPNT is an output variable whose value is the index of the last
C element of RPNT used for the list of points.
C
C LOPN is the length of a point node in RPNT.
C
C IEDG is a one-dimensional output array of type INTEGER in which the
C list of the edges of the triangular mesh is placed.
C
C MEDG is an input expression of type INTEGER specifying the length of
C IEDG.
C
C NEDG is an output variable whose value is the index of the last
C element of IEDG used for the list of edges.
C
C LOEN is the length of an edge node in IEDG.
C
C ITRI is a one-dimensional output array of type INTEGER in which the
C list of the triangles of the triangular mesh is placed.
C
C MTRI is an input expression of type INTEGER specifying the length of
C ITRI.
C
C NTRI is an output variable whose value is the index of the last
C element of ITRI used for the list of triangles.
C
C LOTN is the length of a triangle node in IEDG.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TRMRGR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Build structures forming the triangular mesh.  First, zero the count
C of points, edges, and triangles formed.
C
        NPNT=0
        NEDG=0
        NTRI=0
C
C Initialize the array that keeps track of where in the triangular mesh
C the points and edges of the original rectangular grid were put.
C
        DO 103 I=1,IDIM
          DO 102 J=1,JDIM
            DO 101 K=1,4
              ISCR(I,J,K)=-1
  101       CONTINUE
  102     CONTINUE
  103   CONTINUE
C
C Loop through the cells of the rectangular grid.
C
        DO 106 I=1,IDIM-1
C
          DO 105 J=1,JDIM-1
C
C Use only rectangular cells with data at each of their four corners.
C
            IF (RDAT(I  ,J  ).NE.SVAL.AND.
     +          RDAT(I+1,J  ).NE.SVAL.AND.
     +          RDAT(I  ,J+1).NE.SVAL.AND.
     +          RDAT(I+1,J+1).NE.SVAL) THEN
C
C Within each rectangular cell, loop to produce the two triangles into
C which it will be divided.
C
              DO 104 K=0,1
C
C The cell is split into triangles using one of the two diagonals.  The
C following code determines which diagonal to use; it creates a sort of
C checkerboard pattern, using the diagonal from lower left to upper
C right on cells of one "color" and that from upper left to lower right
C on cells of the other "color".  The logic can be changed to use other
C patterns, but it is important that the points of each triangle be
C specified in counterclockwise order.  We have to take action near the
C poles to avoid forming degenerate triangles there.
C
                IF (MOD(I+J,2).EQ.0) THEN
                  IF (K.EQ.0) THEN
                    INI1=I
                    INJ1=J
                    INI2=I+1
                    INJ2=J
                    INI3=I
                    INJ3=J+1
                  ELSE
                    INI1=I
                    INJ1=J+1
                    INI2=I+1
                    INJ2=J
                    INI3=I+1
                    INJ3=J+1
                  END IF
                ELSE
                  IF (K.EQ.0) THEN
                    INI1=I
                    INJ1=J
                    INI2=I+1
                    INJ2=J+1
                    INI3=I
                    INJ3=J+1
                  ELSE
                    INI1=I
                    INJ1=J
                    INI2=I+1
                    INJ2=J
                    INI3=I+1
                    INJ3=J+1
                  END IF
                END IF
C
C Deal with the first point of the triangle.  We are careful not to put
C the point into the structure more than once.  (That way, we can test
C to see if two edges contain the same point by looking at pointers; we
C don't have to look at coordinates.)
C
                IF (ISCR(INI1,INJ1,4).GE.0) THEN
                  IPP1=ISCR(INI1,INJ1,4)
                ELSE IF (NPNT+LOPN.GT.MPNT) THEN
                  CALL SETER ('TRMRGR - POINT ARRAY IS TOO SMALL',2,1)
                  RETURN
                ELSE
                  IPP1=NPNT
                  NPNT=NPNT+LOPN
                  ISCR(INI1,INJ1,4)=IPP1
                END IF
C
                RPNT(IPP1+1)=XARR(INI1,INJ1)
                RPNT(IPP1+2)=YARR(INI1,INJ1)
                RPNT(IPP1+3)=0
                RPNT(IPP1+4)=RDAT(INI1,INJ1)
C
C Deal with the second point of the triangle.
C
                IF (ISCR(INI2,INJ2,4).GE.0) THEN
                  IPP2=ISCR(INI2,INJ2,4)
                ELSE IF (NPNT+LOPN.GT.MPNT) THEN
                  CALL SETER ('TRMRGR - POINT ARRAY IS TOO SMALL',3,1)
                  RETURN
                ELSE
                  IPP2=NPNT
                  NPNT=NPNT+LOPN
                  ISCR(INI2,INJ2,4)=IPP2
                END IF
C
                RPNT(IPP2+1)=XARR(INI2,INJ2)
                RPNT(IPP2+2)=YARR(INI2,INJ2)
                RPNT(IPP2+3)=0
                RPNT(IPP2+4)=RDAT(INI2,INJ2)
C
C Deal with the third point of the triangle.
C
                IF (ISCR(INI3,INJ3,4).GE.0) THEN
                  IPP3=ISCR(INI3,INJ3,4)
                ELSE IF (NPNT+LOPN.GT.MPNT) THEN
                  CALL SETER ('TRMRGR - POINT ARRAY IS TOO SMALL',4,1)
                  RETURN
                ELSE
                  IPP3=NPNT
                  NPNT=NPNT+LOPN
                  ISCR(INI3,INJ3,4)=IPP3
                END IF
C
                RPNT(IPP3+1)=XARR(INI3,INJ3)
                RPNT(IPP3+2)=YARR(INI3,INJ3)
                RPNT(IPP3+3)=0
                RPNT(IPP3+4)=RDAT(INI3,INJ3)
C
C Deal with the first edge of the triangle (joining points 1 and 2).
C Again, we are careful not to put an edge into the structure more
C than once.  (That way, two triangles that share an edge contain
C pointers to the same edge.)
C
                IF (INI1.EQ.INI2) THEN
                  ITYP=1
                ELSE IF (INJ1.EQ.INJ2) THEN
                  ITYP=2
                ELSE
                  ITYP=3
                END IF
C
                INIM=MIN(INI1,INI2)
                INJM=MIN(INJ1,INJ2)
C
                IF (ISCR(INIM,INJM,ITYP).GE.0) THEN
                  IPE1=ISCR(INIM,INJM,ITYP)
                  IEDG(IPE1+4)=NTRI+1
                ELSE IF (NEDG+LOEN.GT.MEDG) THEN
                  CALL SETER ('TRMRGR - EDGE ARRAY IS TOO SMALL',5,1)
                  RETURN
                ELSE
                  IPE1=NEDG
                  NEDG=NEDG+LOEN
                  ISCR(INIM,INJM,ITYP)=IPE1
                  IEDG(IPE1+1)=IPP1
                  IEDG(IPE1+2)=IPP2
                  IEDG(IPE1+3)=NTRI+1
                  IEDG(IPE1+4)=-1
                END IF
C
C Deal with the second edge of the triangle (joining points 2 and 3).
C
                IF (INI2.EQ.INI3) THEN
                  ITYP=1
                ELSE IF (INJ2.EQ.INJ3) THEN
                  ITYP=2
                ELSE
                  ITYP=3
                END IF
C
                INIM=MIN(INI2,INI3)
                INJM=MIN(INJ2,INJ3)
C
                IF (ISCR(INIM,INJM,ITYP).GE.0) THEN
                  IPE2=ISCR(INIM,INJM,ITYP)
                  IEDG(IPE2+4)=NTRI+2
                ELSE IF (NEDG+LOEN.GT.MEDG) THEN
                  CALL SETER ('TRMRGR - EDGE ARRAY IS TOO SMALL',6,1)
                  RETURN
                ELSE
                  IPE2=NEDG
                  NEDG=NEDG+LOEN
                  ISCR(INIM,INJM,ITYP)=IPE2
                  IEDG(IPE2+1)=IPP2
                  IEDG(IPE2+2)=IPP3
                  IEDG(IPE2+3)=NTRI+2
                  IEDG(IPE2+4)=-1
                END IF
C
C Deal with the third edge of the triangle (joining points 3 and 1).
C
                IF (INI3.EQ.INI1) THEN
                  ITYP=1
                ELSE IF (INJ3.EQ.INJ1) THEN
                  ITYP=2
                ELSE
                  ITYP=3
                END IF
C
                INIM=MIN(INI3,INI1)
                INJM=MIN(INJ3,INJ1)
C
                IF (ISCR(INIM,INJM,ITYP).GE.0) THEN
                  IPE3=ISCR(INIM,INJM,ITYP)
                  IEDG(IPE3+4)=NTRI+3
                ELSE IF (NEDG+LOEN.GT.MEDG) THEN
                  CALL SETER ('TRMRGR - EDGE ARRAY IS TOO SMALL',7,1)
                  RETURN
                ELSE
                  IPE3=NEDG
                  NEDG=NEDG+LOEN
                  ISCR(INIM,INJM,ITYP)=IPE3
                  IEDG(IPE3+1)=IPP3
                  IEDG(IPE3+2)=IPP1
                  IEDG(IPE3+3)=NTRI+3
                  IEDG(IPE3+4)=-1
                END IF
C
C Finally, add the triangle itself to the triangle list.
C
                IF (NTRI+LOTN.GT.MTRI) THEN
                  CALL SETER('TRMRGR - TRIANGLE ARRAY IS TOO SMALL',8,1)
                  RETURN
                ELSE
                  IPTT=NTRI
                  NTRI=NTRI+LOTN
                  ITRI(IPTT+1)=IPE1
                  ITRI(IPTT+2)=IPE2
                  ITRI(IPTT+3)=IPE3
                  ITRI(IPTT+4)=0
                END IF
C
  104         CONTINUE
C
            END IF
C
  105     CONTINUE
C
  106   CONTINUE
C
C Done.
C
      RETURN
C
      END
