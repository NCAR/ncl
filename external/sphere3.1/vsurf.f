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
      SUBROUTINE VSURF(XEYE,YEYE,ZEYE,NTRI,X1,Y1,Z1,X2,Y2,Z2,
     1                 X3,Y3,Z3,ITYPE,WORK,IWORK)
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
      dimension x1(ntri),y1(ntri),z1(ntri),x2(ntri),y2(ntri),z2(ntri),
     1          x3(ntri),y3(ntri),z3(ntri),itype(ntri),work(19*ntri)
      INTEGER IWORK(19*NTRI)
c
      call vsurf1(xeye,yeye,zeye,ntri,x1,y1,z1,x2,y2,z2,x3,y3,z3,
     1 itype,work,work(ntri+1),work(2*ntri+1),work(3*ntri+1),
     2 work(4*ntri+1),work(5*ntri+1),work(6*ntri+1),work(7*ntri+1),
     3 work(8*ntri+1),work(9*ntri+1),work(10*ntri+1),work(11*ntri+1),
     3 work(12*ntri+1),work(13*ntri+1),IWORK(14*NTRI+1),IWORK(6*NTRI+1),
     4 IWORK(15*NTRI+1),IWORK(17*NTRI+1))
      return
      end
