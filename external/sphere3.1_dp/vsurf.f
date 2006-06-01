c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
      SUBROUTINE DVSURF(XEYE,YEYE,ZEYE,NTRI,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,
     +                 ITYPE,WORK,IWORK)
      DOUBLE PRECISION XEYE
      DOUBLE PRECISION YEYE
      DOUBLE PRECISION ZEYE
      DOUBLE PRECISION X1
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Z1
      DOUBLE PRECISION X2
      DOUBLE PRECISION Y2
      DOUBLE PRECISION Z2
      DOUBLE PRECISION X3
      DOUBLE PRECISION Y3
      DOUBLE PRECISION Z3
      DOUBLE PRECISION WORK
c
c    subroutine vsurf is like subroutine hidel except the triangles
c    are categorized. vsurf is also like solid except triangles rather
c    than lines are covered.
c
c     written by paul n. swarztrauber, national center for atmospheric
c     research, p.o. box 3000, boulder, colorado, 80307
c
c    this program plots visible lines for the surface defined
c    by the input 3-d triangles with corners at (x1,y1,z1), (x2,y2,z2)
c    and (x3,y3,z3). the sides of these these triangles may or
c    may not be plotted depending on itype. if itype is 1 then the
c    side between points (x1,y1,z1) and (x2,y2,z2) is plotted if it
c    is visible. if itype is 2 then the side between (x2,y2,z2)
c    and (x3,y3,z3) is plotted. if itype is 3 then the visible portion
c    of the side between (x3,y3,z3) and (x1,y1,z1) is plotted.
c    any combination is possible by specifying itype to be one
c    of the following values: 0,1,2,3,12,13,23,123.
c
c    the length of real    array  work must be at least 19*ntri
c
c    the length of integer array iwork must be at least 19*ntri
c
c
c    the vertices of the triangles are renumbered by vsurf so that
c    their projections are orientated counterclockwise. the user need
c    only be aware that the vertices may be renumbered by vsurf.
c
      DIMENSION X1(NTRI),Y1(NTRI),Z1(NTRI),X2(NTRI),Y2(NTRI),Z2(NTRI),
     +          X3(NTRI),Y3(NTRI),Z3(NTRI),ITYPE(NTRI),WORK(19*NTRI)
      INTEGER IWORK(19*NTRI)
c
      CALL DVSURF1(XEYE,YEYE,ZEYE,NTRI,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,
     +             ITYPE,WORK,WORK(NTRI+1),WORK(2*NTRI+1),
     +             WORK(3*NTRI+1),WORK(4*NTRI+1),WORK(5*NTRI+1),
     +             WORK(6*NTRI+1),WORK(7*NTRI+1),WORK(8*NTRI+1),
     +             WORK(9*NTRI+1),WORK(10*NTRI+1),WORK(11*NTRI+1),
     +             WORK(12*NTRI+1),WORK(13*NTRI+1),IWORK(14*NTRI+1),
     +             IWORK(6*NTRI+1),IWORK(15*NTRI+1),IWORK(17*NTRI+1))
      RETURN
      END
