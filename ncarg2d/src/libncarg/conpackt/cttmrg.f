C
C $Id: cttmrg.f,v 1.5 2004-03-19 22:51:58 kennison Exp $
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
      SUBROUTINE CTTMRG (IDIM,JDIM,RLAT,RLON,RDAT,ISCR,SVAL,RTMI,
     +                   RPNT,MPNT,NPNT,LOPN,
     +                   IEDG,MEDG,NEDG,LOEN,
     +                   ITRI,MTRI,NTRI,LOTN)
C
      DIMENSION RLAT(IDIM,JDIM),RLON(IDIM,JDIM),RDAT(IDIM,JDIM)
      DIMENSION ISCR(IDIM,JDIM,4)
      DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI)
C
C Given arrays defining a rectangular mesh of data deformed to wrap arou
C the globe, CTTMRG returns a triangular mesh representing the data.
C
C The arguments are as follows:
C
C IDIM - an input expression of type INTEGER - the first dimension of
C the rectangular mesh.
C
C JDIM - an input expression of type INTEGER - the second dimension of
C the rectangular mesh.
C
C RLAT - an input array of type REAL, dimensioned IDIM by JDIM - the
C values of latitude for the points of the rectangular mesh.
C
C RLON - an input array of type REAL, dimensioned IDIM by JDIM - the
C values of longitude for the points of the rectangular mesh.
C
C RDAT - an input array of type REAL, dimensioned IDIM by JDIM - the
C values of the data field for the points of the rectangular mesh.
C
C ISCR - a scratch array of type INTEGER, dimensioned IDIM*JDIM*4.
C
C SVAL - an input expression of type REAL - a value which, if used in
C the array RDAT, marks that datum as "special" or "missing".
C
C RTMI - the name of a routine to be called by CTTMRG to determine the
C mapping of the indices of the mesh.  It must be declared EXTERNAL in
C the routine that calls CTTMRG.  The routine must be callable using a
C FORTRAN statement like this:
C
C       CALL RTMI (IDIM,JDIM,IINI,JINI,IINO,JINO)
C
C The arguments IDIM and JDIM are as defined above.  The arguments IINI
C and JINI are input expressions of type INTEGER defining the indices of
C a particular point of the rectangular mesh (1.LE.IINI.LE.IDIM and
C 1.LE.JINI.LE.JDIM).  The arguments IINO and JINO are output variables
C of type INTEGER, that receive the values to be used for the specified
C point of the mesh instead of IINI and JINI.  For example, if the
C rectangular mesh wraps around the globe in such a way that the entire
C first and last rows of the mesh each map into a single point (perhaps
C the south pole and the north pole, respectively) and the left and
C right edges of the mesh are coincident on the globe, then one would
C define RTMI as follows:
C
C     SUBROUTINE RTMI (IDIM,JDIM,IINI,JINI,IINO,JINO)
C
C       IF (JINI.EQ.1) THEN          !  point in first row of mesh
C         IINO=1
C         JINO=1
C       ELSE IF (JINI.EQ.JDIM) THEN  !  point in last row of mesh
C         IINO=1
C         JINO=JDIM
C       ELSE IF (IINI.EQ.IDIM) THEN  !  point in last column of mesh
C         IINO=1
C         JINO=JINI
C       ELSE                         !  all other points of the mesh
C         IINO=IINI
C         JINO=JINI
C       END IF
C
C       RETURN
C
C     END
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
C Define a constant used to convert from degrees to radians.
C
      DATA DTOR / .017453292519943 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTTMRG - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
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
      DO 10001 I=1,IDIM
        DO 10002 J=1,JDIM
          DO 10003 K=1,4
            ISCR(I,J,K)=-1
10003     CONTINUE
10002   CONTINUE
10001 CONTINUE
C
C Loop through the cells of the rectangular grid.
C
      DO 10004 I=1,IDIM-1
C
        DO 10005 J=1,JDIM-1
C
C Use only rectangular cells with data at each of their four corners.
C
          IF (RDAT(I,J).NE.SVAL.AND.RDAT(I+1,J).NE.SVAL.AND.RDAT(I,J+1).
     +NE.SVAL.AND.RDAT(I+1,J+1).NE.SVAL) THEN
C
C Within each rectangular cell, loop to produce the two triangles into
C which it will be divided.
C
            DO 10006 K=0,1
C
C The cell is split into triangles using one of the two diagonals.  The
C following code determines which diagonal to use; it creates a sort of
C checkerboard pattern, using the diagonal from lower left to upper
C right on cells of one "color" and that from upper left to lower right
C on cells of the other "color".  The logic can be changed to use other
C patterns, but it is important that the points of each triangle be
C specified in counterclockwise order.
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
C Find out from the user's index-mapping routine what indices to use for
C the three points.
C
              CALL RTMI (IDIM,JDIM,INI1,INJ1,IOI1,IOJ1)
              CALL RTMI (IDIM,JDIM,INI2,INJ2,IOI2,IOJ2)
              CALL RTMI (IDIM,JDIM,INI3,INJ3,IOI3,IOJ3)
C
C Skip the triangle if any two points of it are coincident (because then
C it's just a line).
C
              IF (IOI1.EQ.IOI2.AND.IOJ1.EQ.IOJ2) GO TO 107
              IF (IOI2.EQ.IOI3.AND.IOJ2.EQ.IOJ3) GO TO 107
              IF (IOI3.EQ.IOI1.AND.IOJ3.EQ.IOJ1) GO TO 107
C
C Skip the triangle if its points all lie too nearly on the same great
C circle.
C
              ANGL=CTABGC(RLAT(IOI1,IOJ1),RLON(IOI1,IOJ1),
     +                    RLAT(IOI2,IOJ2),RLON(IOI2,IOJ2),
     +                    RLAT(IOI3,IOJ3),RLON(IOI3,IOJ3))
C
              IF (ANGL.LT..1.OR.ANGL.GT.179.9) GO TO 107
C
C Deal with the first point of the triangle.  We are careful not to put
C the point into the structure more than once.  (That way, we can test
C to see if two edges contain the same point by looking at pointers; we
C don't have to look at coordinates.)
C
              IF (ISCR(IOI1,IOJ1,4).GE.0) THEN
                IPP1=ISCR(IOI1,IOJ1,4)
              ELSE IF (NPNT+LOPN.GT.MPNT) THEN
                CALL SETER ('CTTMRG - POINT ARRAY IS TOO SMALL',2,1)
                RETURN
              ELSE
                IPP1=NPNT
                NPNT=NPNT+LOPN
                ISCR(IOI1,IOJ1,4)=IPP1
              END IF
C
              RPNT(IPP1+1)=COS(DTOR*RLAT(IOI1,IOJ1))*
     +                     COS(DTOR*RLON(IOI1,IOJ1))
              RPNT(IPP1+2)=COS(DTOR*RLAT(IOI1,IOJ1))*
     +                     SIN(DTOR*RLON(IOI1,IOJ1))
              RPNT(IPP1+3)=SIN(DTOR*RLAT(IOI1,IOJ1))
              RPNT(IPP1+4)=         RDAT(IOI1,IOJ1)
C
C Deal with the second point of the triangle.
C
              IF (ISCR(IOI2,IOJ2,4).GE.0) THEN
                IPP2=ISCR(IOI2,IOJ2,4)
              ELSE IF (NPNT+LOPN.GT.MPNT) THEN
                CALL SETER ('CTTMRG - POINT ARRAY IS TOO SMALL',3,1)
                RETURN
              ELSE
                IPP2=NPNT
                NPNT=NPNT+LOPN
                ISCR(IOI2,IOJ2,4)=IPP2
              END IF
C
              RPNT(IPP2+1)=COS(DTOR*RLAT(IOI2,IOJ2))*
     +                     COS(DTOR*RLON(IOI2,IOJ2))
              RPNT(IPP2+2)=COS(DTOR*RLAT(IOI2,IOJ2))*
     +                     SIN(DTOR*RLON(IOI2,IOJ2))
              RPNT(IPP2+3)=SIN(DTOR*RLAT(IOI2,IOJ2))
              RPNT(IPP2+4)=         RDAT(IOI2,IOJ2)
C
C Deal with the third point of the triangle.
C
              IF (ISCR(IOI3,IOJ3,4).GE.0) THEN
                IPP3=ISCR(IOI3,IOJ3,4)
              ELSE IF (NPNT+LOPN.GT.MPNT) THEN
                CALL SETER ('CTTMRG - POINT ARRAY IS TOO SMALL',4,1)
                RETURN
              ELSE
                IPP3=NPNT
                NPNT=NPNT+LOPN
                ISCR(IOI3,IOJ3,4)=IPP3
              END IF
C
              RPNT(IPP3+1)=COS(DTOR*RLAT(IOI3,IOJ3))*
     +                     COS(DTOR*RLON(IOI3,IOJ3))
              RPNT(IPP3+2)=COS(DTOR*RLAT(IOI3,IOJ3))*
     +                     SIN(DTOR*RLON(IOI3,IOJ3))
              RPNT(IPP3+3)=SIN(DTOR*RLAT(IOI3,IOJ3))
              RPNT(IPP3+4)=         RDAT(IOI3,IOJ3)
C
C Deal with the first edge of the triangle (joining points 1 and 2).
C Again, we are careful not to put an edge into the structure more
C than once.  (That way, two triangles that share an edge contain
C pointers to the same edge.)  The logic here is a bit opaque; if
C the user routine RTMI maps the edge to some edge of the unmapped
C triangular mesh, we can keep track of its index using that element
C of ISCR reserved for that edge; otherwise, we just have to search
C the whole edge list to see if the edge is already there (which
C shouldn't happen too often).
C
              IF (ABS(IOI1-IOI2).LE.1.AND.ABS(IOJ1-IOJ2).LE.1) THEN
                IDIR=(IOI1-IOI2)*(IOJ1-IOJ2)
                IVOK=MOD(MIN(IOI1,IOI2)+MIN(IOJ1,IOJ2),2)
                IF ((IDIR.EQ.0).OR.(IDIR.LT.0.AND.IVOK.EQ.0).OR.(IDIR.GT
     +.0.AND.IVOK.EQ.1)) THEN
                  IF (IOI1.EQ.IOI2) THEN
                    ITYP=1
                  ELSE IF (IOJ1.EQ.IOJ2) THEN
                    ITYP=2
                  ELSE
                    ITYP=3
                  END IF
                  IOIM=MIN(IOI1,IOI2)
                  IOJM=MIN(IOJ1,IOJ2)
                  IF (ISCR(IOIM,IOJM,ITYP).GE.0) THEN
                    IPE1=ISCR(IOIM,IOJM,ITYP)
                    IEDG(IPE1+4)=NTRI+1
                    GO TO 102
                  END IF
                  ISCR(IOIM,IOJM,ITYP)=NEDG
                  GO TO 101
                END IF
              END IF
              DO 10007 IPTE=0,NEDG-LOEN,LOEN
                IF ((IEDG(IPTE+1).EQ.IPP2.AND.IEDG(IPTE+2).EQ.IPP1)) THE
     +N
                  IPE1=IPTE
                  IEDG(IPE1+4)=NTRI+1
                  GO TO 102
                END IF
10007         CONTINUE
  101         IF (NEDG+LOEN.GT.MEDG) THEN
                CALL SETER ('CTTMRG - EDGE ARRAY IS TOO SMALL',5,1)
                RETURN
              ELSE
                IPE1=NEDG
                NEDG=NEDG+LOEN
                IEDG(IPE1+1)=IPP1
                IEDG(IPE1+2)=IPP2
                IEDG(IPE1+3)=NTRI+1
                IEDG(IPE1+4)=-1
              END IF
C
C Deal with the second edge of the triangle (joining points 2 and 3).
C
  102         IF (ABS(IOI2-IOI3).LE.1.AND.ABS(IOJ2-IOJ3).LE.1) THEN
                IDIR=(IOI2-IOI3)*(IOJ2-IOJ3)
                IVOK=MOD(MIN(IOI2,IOI3)+MIN(IOJ2,IOJ3),2)
                IF ((IDIR.EQ.0).OR.(IDIR.LT.0.AND.IVOK.EQ.0).OR.(IDIR.GT
     +.0.AND.IVOK.EQ.1)) THEN
                  IF (IOI2.EQ.IOI3) THEN
                    ITYP=1
                  ELSE IF (IOJ2.EQ.IOJ3) THEN
                    ITYP=2
                  ELSE
                    ITYP=3
                  END IF
                  IOIM=MIN(IOI2,IOI3)
                  IOJM=MIN(IOJ2,IOJ3)
                  IF (ISCR(IOIM,IOJM,ITYP).GE.0) THEN
                    IPE2=ISCR(IOIM,IOJM,ITYP)
                    IEDG(IPE2+4)=NTRI+2
                    GO TO 104
                  END IF
                  ISCR(IOIM,IOJM,ITYP)=NEDG
                  GO TO 103
                END IF
              END IF
              DO 10008 IPTE=0,NEDG-LOEN,LOEN
                IF ((IEDG(IPTE+1).EQ.IPP3.AND.IEDG(IPTE+2).EQ.IPP2)) THE
     +N
                  IPE2=IPTE
                  IEDG(IPE2+4)=NTRI+2
                  GO TO 104
                END IF
10008         CONTINUE
  103         IF (NEDG+LOEN.GT.MEDG) THEN
                CALL SETER ('CTTMRG - EDGE ARRAY IS TOO SMALL',6,1)
                RETURN
              ELSE
                IPE2=NEDG
                NEDG=NEDG+LOEN
                IEDG(IPE2+1)=IPP2
                IEDG(IPE2+2)=IPP3
                IEDG(IPE2+3)=NTRI+2
                IEDG(IPE2+4)=-1
              END IF
C
C Deal with the third edge of the triangle (joining points 3 and 1).
C
  104         IF (ABS(IOI3-IOI1).LE.1.AND.ABS(IOJ3-IOJ1).LE.1) THEN
                IDIR=(IOI3-IOI1)*(IOJ3-IOJ1)
                IVOK=MOD(MIN(IOI3,IOI1)+MIN(IOJ3,IOJ1),2)
                IF ((IDIR.EQ.0).OR.(IDIR.LT.0.AND.IVOK.EQ.0).OR.(IDIR.GT
     +.0.AND.IVOK.EQ.1)) THEN
                  IF (IOI3.EQ.IOI1) THEN
                    ITYP=1
                  ELSE IF (IOJ3.EQ.IOJ1) THEN
                    ITYP=2
                  ELSE
                    ITYP=3
                  END IF
                  IOIM=MIN(IOI3,IOI1)
                  IOJM=MIN(IOJ3,IOJ1)
                  IF (ISCR(IOIM,IOJM,ITYP).GE.0) THEN
                    IPE3=ISCR(IOIM,IOJM,ITYP)
                    IEDG(IPE3+4)=NTRI+3
                    GO TO 106
                  END IF
                  ISCR(IOIM,IOJM,ITYP)=NEDG
                  GO TO 105
                END IF
              END IF
              DO 10009 IPTE=0,NEDG-LOEN,LOEN
                IF ((IEDG(IPTE+1).EQ.IPP1.AND.IEDG(IPTE+2).EQ.IPP3)) THE
     +N
                  IPE3=IPTE
                  IEDG(IPE3+4)=NTRI+3
                  GO TO 106
                END IF
10009         CONTINUE
  105         IF (NEDG+LOEN.GT.MEDG) THEN
                CALL SETER ('CTTMRG - EDGE ARRAY IS TOO SMALL',7,1)
                RETURN
              ELSE
                IPE3=NEDG
                NEDG=NEDG+LOEN
                IEDG(IPE3+1)=IPP3
                IEDG(IPE3+2)=IPP1
                IEDG(IPE3+3)=NTRI+3
                IEDG(IPE3+4)=-1
              END IF
C
C Finally, add the triangle itself to the triangle list.
C
  106         IF (NTRI+LOTN.GT.MTRI) THEN
                CALL SETER ('CTTMRG - TRIANGLE ARRAY IS TOO SMALL',
     +                                                            8,1)
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
  107       CONTINUE
10006       CONTINUE
C
          END IF
C
10005   CONTINUE
C
10004 CONTINUE
C
C Done.
C
      RETURN
C
      END
