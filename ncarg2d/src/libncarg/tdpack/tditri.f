C
C $Id: tditri.f,v 1.2 2000-07-12 16:26:33 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE TDITRI (U,NU,V,NV,W,NW,F,LF1D,LF2D,FISO,
     +                               RTRI,MTRI,NTRI,IRST)
C
        DIMENSION U(NU),V(NV),W(NW),F(LF1D,LF2D,*),RTRI(10,MTRI)
C
C The routine TDITRI, given a 3-dimensional array of function values in
C the array F and a cutoff value FISO, generates a set of triangles
C representing the surface defined by those values and adds them to a
C list of triangles tendered by the user.  The arguments are as follows:
C
C   U is an input array of real values of the independent variable U.
C
C   NU is an input integer specifying the dimension of the array U.
C
C   V is an input array of real values of the independent variable V.
C
C   NV is an input integer specifying the dimension of the array V.
C
C   W is an input array of real values of the independent variable W.
C
C   NW is an input integer specifying the dimension of the array W.
C
C   F is the 3-dimensional real array of function values.  For each I
C   from 1 to NU, each J from 1 to NV, and each K from 1 to NW, F(I,J,K)
C   is the value of the function F at the point (U(I),V(J),W(K)).
C
C   LF1D is an input integer specifying the first (FORTRAN) dimension of
C   the array F.  It must be the case that LF1D is greater than or equal
C   to NU.
C
C   LF2D is an input integer specifying the second (FORTRAN) dimension
C   of the array F.  It must be the case that LF1D is greater than or
C   equal to NV.
C
C   FISO is the cutoff value defining the isosurface.  The object of
C   a call to TDITRI is to generate a set of triangles separating the
C   3-space box within which the function F is defined into two volumes:
C   one where the value of F is less than or equal to FISO and another
C   where the value of F is greater than FISO.
C
C   RTRI is a real array in which a list of triangles is being
C   generated; it is dimensioned 10 x MTRI.  Each 10-word entry in
C   the list consists of the U, V, and W coordinates of each of three
C   points, followed by a rendering style index.
C
C   MTRI is an input integer specifying the second dimension of the
C   array RTRI: the maximum number of triangles the list will hold.
C
C   NTRI is an input/output integer specifying the number of triangles
C   currently in the array RTRI.  Initially, NTRI must be zeroed by the
C   user.  Each call to one of the routines TDSTRI or TDITRI updates the
C   value of NTRI to reflect the number of triangles added to the list.
C
C   IRST is an input integer specifying the rendering style index to be
C   used for all the triangles generated by a particular call to TDITRI.
C   Note that more than one surface can be represented by the triangles
C   in the triangle list and that each surface can be rendered in its
C   own particular style.
C
C This routine deals, one at a time, with each cell of the 3-dimensional
C grid.  The following picture shows the order in which the corners of a
C cell are numbered:
C
C         7-------8
C        /|      /|
C       5-------6 |
C       | |     | |
C       | 3-----|-4
C       |/      |/
C       1-------2
C
C The U, V, and W coordinates defining each of the eight corners of such
C a cell are generated in the arrays BOXU, BOXV, and BOXW; the function
C values at each of those corners are put in BOXF.
C
        DIMENSION BOXU(8),BOXV(8),BOXW(8),BOXF(8)
C
C The arrays IOSU, IOSV, and IOSW specify index offsets to be used in
C setting elements of BOXU, BOXV, BOXW, and BOXF and are defined in a
C DATA statement below.
C
        DIMENSION IOSU(8),IOSV(8),IOSW(8)
C
C Because, at each corner of a cell, the value of F can be either less
C than or equal to FISO or greater than FISO, and because the cell has
C eight corners, there are 256 different cases to be considered.  Each
C of these can be associated with a particular eight-bit binary integer
C (between 0 and 255) in which each bit represents a particular corner
C of the cell; the bit is set to 1 if and only if the function value at
C that corner is less than or equal to FISO.  One need only consider the
C first 128 of these cases: for I greater than 127, case I is equivalent
C to case 255-I ("greater than or equal to" and "less than" are swapped
C with each other).  Observing that case 0 is trivial (no triangles),
C we can then consider only 127 cases numbered 1 through 127.  It turns
C out, by rotating the cell appropriately, each of these 127 cases can
C be reduced to one of 14 basic cases.  For each I, ABS(IACT(I))
C specifies which of the 14 basic cases case I reduces to and the values
C (IPER(L,I),L=1,8) specify which corners of the cell have to be treated
C as corners 1 through 8.  The sign of IACT(I) says whether triangles to
C be generated for case I mirror images of the triangles generated for
C basic case ABS(IACT(I)).
C
        DIMENSION IACT(127),IPER(8,127)
C
C Now, for each value of I from 1 to 14, inclusive, LBEG(I) and LEND(I)
C define the first and last entries in the arrays IP11, IP12, IP21,
C IP22, IP31, and IP32 that pertain to basic case I: for each L between
C LBEG(I) and LEND(I), three triangle corner points are defined -
C one obtained by interpolating between the points IP11(L) and IP12(L),
C another obtained by interpolating between the points IP21(L) and
C IP22(L), and a third obtained by interpolating between the points
C IP31(L) and IP32(L).
C
        DIMENSION LBEG(14),LEND(14)
C
        DIMENSION IP11(52),IP12(52),IP21(52),IP22(52),IP31(52),IP32(52)
C
C Define index offsets for corner points of a grid cell.
C
        DATA IOSU / 0,1,0,1,0,1,0,1 /
        DATA IOSV / 0,0,1,1,0,0,1,1 /
        DATA IOSW / 0,0,0,0,1,1,1,1 /
C
C Define IACT and IPER for 127 cases.
C
        DATA IACT(  1),(IPER(I,  1),I=1,8) /   1 , 1,2,3,4,5,6,7,8 /
        DATA IACT(  2),(IPER(I,  2),I=1,8) /   1 , 2,4,1,3,6,8,5,7 /
        DATA IACT(  3),(IPER(I,  3),I=1,8) /   2 , 1,2,3,4,5,6,7,8 /
        DATA IACT(  4),(IPER(I,  4),I=1,8) /   1 , 3,1,4,2,7,5,8,6 /
        DATA IACT(  5),(IPER(I,  5),I=1,8) /   2 , 3,1,4,2,7,5,8,6 /
        DATA IACT(  6),(IPER(I,  6),I=1,8) /   3 , 1,2,3,4,5,6,7,8 /
        DATA IACT(  7),(IPER(I,  7),I=1,8) /   4 , 1,2,3,4,5,6,7,8 /
        DATA IACT(  8),(IPER(I,  8),I=1,8) /   1 , 4,3,2,1,8,7,6,5 /
        DATA IACT(  9),(IPER(I,  9),I=1,8) /   3 , 3,1,4,2,7,5,8,6 /
        DATA IACT( 10),(IPER(I, 10),I=1,8) /   2 , 2,4,1,3,6,8,5,7 /
        DATA IACT( 11),(IPER(I, 11),I=1,8) /   4 , 2,4,1,3,6,8,5,7 /
        DATA IACT( 12),(IPER(I, 12),I=1,8) /   2 , 4,3,2,1,8,7,6,5 /
        DATA IACT( 13),(IPER(I, 13),I=1,8) /   4 , 3,1,4,2,7,5,8,6 /
        DATA IACT( 14),(IPER(I, 14),I=1,8) /   4 , 4,3,2,1,8,7,6,5 /
        DATA IACT( 15),(IPER(I, 15),I=1,8) /   5 , 1,2,3,4,5,6,7,8 /
        DATA IACT( 16),(IPER(I, 16),I=1,8) /   1 , 5,6,1,2,7,8,3,4 /
        DATA IACT( 17),(IPER(I, 17),I=1,8) /   2 , 5,1,7,3,6,2,8,4 /
        DATA IACT( 18),(IPER(I, 18),I=1,8) /   3 , 6,2,5,1,8,4,7,3 /
        DATA IACT( 19),(IPER(I, 19),I=1,8) /   4 , 1,5,2,6,3,7,4,8 /
        DATA IACT( 20),(IPER(I, 20),I=1,8) /   3 , 7,5,3,1,8,6,4,2 /
        DATA IACT( 21),(IPER(I, 21),I=1,8) /   4 , 1,3,5,7,2,4,6,8 /
        DATA IACT( 22),(IPER(I, 22),I=1,8) /   6 , 1,2,3,4,5,6,7,8 /
        DATA IACT( 23),(IPER(I, 23),I=1,8) /   7 , 1,2,3,4,5,6,7,8 /
        DATA IACT( 24),(IPER(I, 24),I=1,8) /   8 , 1,2,3,4,5,6,7,8 /
        DATA IACT( 25),(IPER(I, 25),I=1,8) /   9 , 1,2,3,4,5,6,7,8 /
        DATA IACT( 26),(IPER(I, 26),I=1,8) /   9 , 2,1,6,5,4,3,8,7 /
        DATA IACT( 27),(IPER(I, 27),I=1,8) /  10 , 1,2,3,4,5,6,7,8 /
        DATA IACT( 28),(IPER(I, 28),I=1,8) /   9 , 3,7,1,5,4,8,2,6 /
        DATA IACT( 29),(IPER(I, 29),I=1,8) /  11 , 1,2,3,4,5,6,7,8 /
        DATA IACT( 30),(IPER(I, 30),I=1,8) /  12 , 1,2,3,4,5,6,7,8 /
        DATA IACT( 31),(IPER(I, 31),I=1,8) /  -4 , 8,6,7,5,4,2,3,1 /
        DATA IACT( 32),(IPER(I, 32),I=1,8) /   1 , 6,8,2,4,5,7,1,3 /
        DATA IACT( 33),(IPER(I, 33),I=1,8) /   3 , 5,6,1,2,7,8,3,4 /
        DATA IACT( 34),(IPER(I, 34),I=1,8) /   2 , 6,2,5,1,8,4,7,3 /
        DATA IACT( 35),(IPER(I, 35),I=1,8) /   4 , 2,1,6,5,4,3,8,7 /
        DATA IACT( 36),(IPER(I, 36),I=1,8) /   8 , 2,4,1,3,6,8,5,7 /
        DATA IACT( 37),(IPER(I, 37),I=1,8) /   9 , 1,5,2,6,3,7,4,8 /
        DATA IACT( 38),(IPER(I, 38),I=1,8) /   9 , 2,4,1,3,6,8,5,7 /
        DATA IACT( 39),(IPER(I, 39),I=1,8) /  11 , 1,5,2,6,3,7,4,8 /
        DATA IACT( 40),(IPER(I, 40),I=1,8) /   3 , 8,4,6,2,7,3,5,1 /
        DATA IACT( 41),(IPER(I, 41),I=1,8) /   6 , 2,4,1,3,6,8,5,7 /
        DATA IACT( 42),(IPER(I, 42),I=1,8) /   4 , 2,6,4,8,1,5,3,7 /
        DATA IACT( 43),(IPER(I, 43),I=1,8) /   7 , 2,4,1,3,6,8,5,7 /
        DATA IACT( 44),(IPER(I, 44),I=1,8) /   9 , 4,2,8,6,3,1,7,5 /
        DATA IACT( 45),(IPER(I, 45),I=1,8) /  12 , 2,4,1,3,6,8,5,7 /
        DATA IACT( 46),(IPER(I, 46),I=1,8) /  10 , 2,4,1,3,6,8,5,7 /
        DATA IACT( 47),(IPER(I, 47),I=1,8) /  -4 , 7,8,5,6,3,4,1,2 /
        DATA IACT( 48),(IPER(I, 48),I=1,8) /   2 , 5,6,1,2,7,8,3,4 /
        DATA IACT( 49),(IPER(I, 49),I=1,8) /   4 , 5,6,1,2,7,8,3,4 /
        DATA IACT( 50),(IPER(I, 50),I=1,8) /   4 , 6,2,5,1,8,4,7,3 /
        DATA IACT( 51),(IPER(I, 51),I=1,8) /   5 , 5,6,1,2,7,8,3,4 /
        DATA IACT( 52),(IPER(I, 52),I=1,8) /   9 , 5,1,7,3,6,2,8,4 /
        DATA IACT( 53),(IPER(I, 53),I=1,8) /  10 , 5,1,7,3,6,2,8,4 /
        DATA IACT( 54),(IPER(I, 54),I=1,8) /  12 , 1,5,2,6,3,7,4,8 /
        DATA IACT( 55),(IPER(I, 55),I=1,8) /  -4 , 8,7,4,3,6,5,2,1 /
        DATA IACT( 56),(IPER(I, 56),I=1,8) /   9 , 6,8,2,4,5,7,1,3 /
        DATA IACT( 57),(IPER(I, 57),I=1,8) /  12 , 2,1,6,5,4,3,8,7 /
        DATA IACT( 58),(IPER(I, 58),I=1,8) /  11 , 2,1,6,5,4,3,8,7 /
        DATA IACT( 59),(IPER(I, 59),I=1,8) /  -4 , 7,3,8,4,5,1,6,2 /
        DATA IACT( 60),(IPER(I, 60),I=1,8) /  13 , 1,2,3,4,5,6,7,8 /
        DATA IACT( 61),(IPER(I, 61),I=1,8) /  -9 , 8,4,6,2,7,3,5,1 /
        DATA IACT( 62),(IPER(I, 62),I=1,8) /  -9 , 7,5,3,1,8,6,4,2 /
        DATA IACT( 63),(IPER(I, 63),I=1,8) /  -2 , 7,8,5,6,3,4,1,2 /
        DATA IACT( 64),(IPER(I, 64),I=1,8) /   1 , 7,8,5,6,3,4,1,2 /
        DATA IACT( 65),(IPER(I, 65),I=1,8) /   3 , 5,1,7,3,6,2,8,4 /
        DATA IACT( 66),(IPER(I, 66),I=1,8) /   8 , 3,1,4,2,7,5,8,6 /
        DATA IACT( 67),(IPER(I, 67),I=1,8) /   9 , 1,3,5,7,2,4,6,8 /
        DATA IACT( 68),(IPER(I, 68),I=1,8) /   2 , 3,7,1,5,4,8,2,6 /
        DATA IACT( 69),(IPER(I, 69),I=1,8) /   4 , 3,7,1,5,4,8,2,6 /
        DATA IACT( 70),(IPER(I, 70),I=1,8) /   9 , 3,1,4,2,7,5,8,6 /
        DATA IACT( 71),(IPER(I, 71),I=1,8) /  10 , 3,1,4,2,7,5,8,6 /
        DATA IACT( 72),(IPER(I, 72),I=1,8) /   3 , 3,4,7,8,1,2,5,6 /
        DATA IACT( 73),(IPER(I, 73),I=1,8) /   6 , 3,1,4,2,7,5,8,6 /
        DATA IACT( 74),(IPER(I, 74),I=1,8) /   9 , 4,8,3,7,2,6,1,5 /
        DATA IACT( 75),(IPER(I, 75),I=1,8) /  12 , 3,1,4,2,7,5,8,6 /
        DATA IACT( 76),(IPER(I, 76),I=1,8) /   4 , 3,4,7,8,1,2,5,6 /
        DATA IACT( 77),(IPER(I, 77),I=1,8) /   7 , 3,1,4,2,7,5,8,6 /
        DATA IACT( 78),(IPER(I, 78),I=1,8) /  11 , 3,1,4,2,7,5,8,6 /
        DATA IACT( 79),(IPER(I, 79),I=1,8) /  -4 , 6,5,8,7,2,1,4,3 /
        DATA IACT( 80),(IPER(I, 80),I=1,8) /   2 , 5,7,6,8,1,3,2,4 /
        DATA IACT( 81),(IPER(I, 81),I=1,8) /   4 , 5,1,7,3,6,2,8,4 /
        DATA IACT( 82),(IPER(I, 82),I=1,8) /   9 , 5,6,1,2,7,8,3,4 /
        DATA IACT( 83),(IPER(I, 83),I=1,8) /  11 , 5,6,1,2,7,8,3,4 /
        DATA IACT( 84),(IPER(I, 84),I=1,8) /   4 , 7,5,3,1,8,6,4,2 /
        DATA IACT( 85),(IPER(I, 85),I=1,8) /   5 , 1,3,5,7,2,4,6,8 /
        DATA IACT( 86),(IPER(I, 86),I=1,8) /  12 , 1,3,5,7,2,4,6,8 /
        DATA IACT( 87),(IPER(I, 87),I=1,8) /  -4 , 8,4,6,2,7,3,5,1 /
        DATA IACT( 88),(IPER(I, 88),I=1,8) /   9 , 7,3,8,4,5,1,6,2 /
        DATA IACT( 89),(IPER(I, 89),I=1,8) /  12 , 3,7,1,5,4,8,2,6 /
        DATA IACT( 90),(IPER(I, 90),I=1,8) /  13 , 1,3,5,7,2,4,6,8 /
        DATA IACT( 91),(IPER(I, 91),I=1,8) /  -9 , 8,7,4,3,6,5,2,1 /
        DATA IACT( 92),(IPER(I, 92),I=1,8) /  10 , 3,7,1,5,4,8,2,6 /
        DATA IACT( 93),(IPER(I, 93),I=1,8) /  -4 , 6,8,2,4,5,7,1,3 /
        DATA IACT( 94),(IPER(I, 94),I=1,8) /  -9 , 6,2,5,1,8,4,7,3 /
        DATA IACT( 95),(IPER(I, 95),I=1,8) /  -2 , 6,8,2,4,5,7,1,3 /
        DATA IACT( 96),(IPER(I, 96),I=1,8) /   3 , 8,6,7,5,4,2,3,1 /
        DATA IACT( 97),(IPER(I, 97),I=1,8) /   6 , 5,6,1,2,7,8,3,4 /
        DATA IACT( 98),(IPER(I, 98),I=1,8) /   9 , 6,5,8,7,2,1,4,3 /
        DATA IACT( 99),(IPER(I, 99),I=1,8) /  12 , 5,6,1,2,7,8,3,4 /
        DATA IACT(100),(IPER(I,100),I=1,8) /   9 , 7,8,5,6,3,4,1,2 /
        DATA IACT(101),(IPER(I,101),I=1,8) /  12 , 5,1,7,3,6,2,8,4 /
        DATA IACT(102),(IPER(I,102),I=1,8) /  13 , 5,1,7,3,6,2,8,4 /
        DATA IACT(103),(IPER(I,103),I=1,8) /  -9 , 8,6,7,5,4,2,3,1 /
        DATA IACT(104),(IPER(I,104),I=1,8) /   6 , 8,7,4,3,6,5,2,1 /
        DATA IACT(105),(IPER(I,105),I=1,8) /  14 , 1,2,3,4,5,6,7,8 /
        DATA IACT(106),(IPER(I,106),I=1,8) /  12 , 8,4,6,2,7,3,5,1 /
        DATA IACT(107),(IPER(I,107),I=1,8) /  -6 , 7,5,3,1,8,6,4,2 /
        DATA IACT(108),(IPER(I,108),I=1,8) /  12 , 8,7,4,3,6,5,2,1 /
        DATA IACT(109),(IPER(I,109),I=1,8) /  -6 , 6,2,5,1,8,4,7,3 /
        DATA IACT(110),(IPER(I,110),I=1,8) /  -9 , 5,7,6,8,1,3,2,4 /
        DATA IACT(111),(IPER(I,111),I=1,8) /  -3 , 7,8,5,6,3,4,1,2 /
        DATA IACT(112),(IPER(I,112),I=1,8) /   4 , 5,7,6,8,1,3,2,4 /
        DATA IACT(113),(IPER(I,113),I=1,8) /   7 , 5,6,1,2,7,8,3,4 /
        DATA IACT(114),(IPER(I,114),I=1,8) /  10 , 5,6,1,2,7,8,3,4 /
        DATA IACT(115),(IPER(I,115),I=1,8) /  -4 , 4,8,3,7,2,6,1,5 /
        DATA IACT(116),(IPER(I,116),I=1,8) /  11 , 5,1,7,3,6,2,8,4 /
        DATA IACT(117),(IPER(I,117),I=1,8) /  -4 , 4,2,8,6,3,1,7,5 /
        DATA IACT(118),(IPER(I,118),I=1,8) /  -9 , 4,3,2,1,8,7,6,5 /
        DATA IACT(119),(IPER(I,119),I=1,8) /  -2 , 4,8,3,7,2,6,1,5 /
        DATA IACT(120),(IPER(I,120),I=1,8) /  12 , 8,6,7,5,4,2,3,1 /
        DATA IACT(121),(IPER(I,121),I=1,8) /  -6 , 4,3,2,1,8,7,6,5 /
        DATA IACT(122),(IPER(I,122),I=1,8) /  -9 , 3,4,7,8,1,2,5,6 /
        DATA IACT(123),(IPER(I,123),I=1,8) /  -3 , 7,3,8,4,5,1,6,2 /
        DATA IACT(124),(IPER(I,124),I=1,8) /  -9 , 2,6,4,8,1,5,3,7 /
        DATA IACT(125),(IPER(I,125),I=1,8) /  -3 , 6,8,2,4,5,7,1,3 /
        DATA IACT(126),(IPER(I,126),I=1,8) /  -8 , 4,3,2,1,8,7,6,5 /
        DATA IACT(127),(IPER(I,127),I=1,8) /  -1 , 8,6,7,5,4,2,3,1 /
C
C Define LBEG and LEND for each of 14 basic cases.
C
        DATA LBEG /  1, 2, 4, 8,11,13,20,24,26,31,35,39,45,49 /
        DATA LEND /  1, 3, 7,10,12,19,23,25,30,34,38,44,48,52 /
C
C Define corner point data.
C
        DATA (IP11(I),I= 1,13) / 1,1,1,1,1,2,2,1,2,2,1,2,1 /
        DATA (IP12(I),I= 1,13) / 2,3,3,2,3,6,6,5,6,4,5,6,2 /
        DATA (IP21(I),I= 1,13) / 1,1,2,2,2,2,3,2,3,3,2,4,2 /
        DATA (IP22(I),I= 1,13) / 3,5,6,6,6,4,4,6,4,4,6,8,6 /
        DATA (IP31(I),I= 1,13) / 1,2,2,1,3,3,3,3,3,2,3,3,1 /
        DATA (IP32(I),I= 1,13) / 5,6,4,3,7,4,7,7,7,6,7,7,3 /
C
        DATA (IP11(I),I=14,26) / 1,1,5,2,2,2,3,2,2,2,1,2,1 /
        DATA (IP12(I),I=14,26) / 3,3,6,6,6,4,7,6,6,4,5,4,2 /
        DATA (IP21(I),I=14,26) / 2,5,2,3,3,3,5,3,3,3,5,4,2 /
        DATA (IP22(I),I=14,26) / 6,6,6,7,4,4,7,7,4,4,7,8,4 /
        DATA (IP31(I),I=14,26) / 5,1,5,5,3,2,5,5,3,2,5,3,5 /
        DATA (IP32(I),I=14,26) / 6,5,7,7,7,6,6,6,7,6,6,4,6 /
C
        DATA (IP11(I),I=27,39) / 5,4,1,3,1,1,1,2,1,1,1,1,1 /
        DATA (IP12(I),I=27,39) / 6,8,3,4,3,3,3,6,2,2,2,2,3 /
        DATA (IP21(I),I=27,39) / 2,5,5,5,5,5,2,4,2,4,3,5,5 /
        DATA (IP22(I),I=27,39) / 4,7,7,7,7,6,6,8,4,8,7,7,6 /
        DATA (IP31(I),I=27,39) / 4,5,3,4,5,2,3,3,4,3,5,5,1 /
        DATA (IP32(I),I=27,39) / 8,6,4,8,6,6,4,4,8,7,7,6,5 /
C
        DATA (IP11(I),I=40,52) / 1,1,2,3,3,1,2,1,2,1,2,2,3 /
        DATA (IP12(I),I=40,52) / 2,3,6,7,7,3,4,5,6,2,4,6,7 /
        DATA (IP21(I),I=40,52) / 2,2,4,5,5,2,4,5,5,1,4,5,7 /
        DATA (IP22(I),I=40,52) / 6,6,8,6,7,4,8,7,7,3,8,6,8 /
        DATA (IP31(I),I=40,52) / 1,5,5,4,5,3,3,2,6,1,3,6,5 /
        DATA (IP32(I),I=40,52) / 3,6,6,8,6,7,7,6,8,5,4,8,7 /
C
C The following arithmetic statement functions are used to generate the
C U, V, and W coordinates of the point lying between box corners ICN1
C and ICN2 where the interpolated value of the function F is equal to
C FISO.
C
        UINT(ICN1,ICN2)=BOXU(ICN1)+(BOXU(ICN2)-BOXU(ICN1))*
     +                             (FISO      -BOXF(ICN1))/
     +                             (BOXF(ICN2)-BOXF(ICN1))
C
        VINT(ICN1,ICN2)=BOXV(ICN1)+(BOXV(ICN2)-BOXV(ICN1))*
     +                             (FISO      -BOXF(ICN1))/
     +                             (BOXF(ICN2)-BOXF(ICN1))
C
        WINT(ICN1,ICN2)=BOXW(ICN1)+(BOXW(ICN2)-BOXW(ICN1))*
     +                             (FISO      -BOXF(ICN1))/
     +                             (BOXF(ICN2)-BOXF(ICN1))
C
C Get the real equivalent of the rendering style index.
C
        RRST=REAL(IRST)
C
C Loop through the cells of the 3-dimensional grid, determining what
C case applies to each cell and generating triangles in the triangle
C list to represent each cell through which the isosurface passes.
C Note that care is taken to trace each triangle counterclockwise as
C viewed from the side toward which the function value increases;
C this is important, as it enables the renderer to tell which side
C of a triangle is being looked at.
C
        DO 106 I=1,NU-1
          DO 105 J=1,NV-1
            DO 104 K=1,NW-1
              ICSE=0
              IF (F(I  ,J  ,K  ).LE.FISO) ICSE=ICSE+1
              IF (F(I+1,J  ,K  ).LE.FISO) ICSE=ICSE+2
              IF (F(I  ,J+1,K  ).LE.FISO) ICSE=ICSE+4
              IF (F(I+1,J+1,K  ).LE.FISO) ICSE=ICSE+8
              IF (F(I  ,J  ,K+1).LE.FISO) ICSE=ICSE+16
              IF (F(I+1,J  ,K+1).LE.FISO) ICSE=ICSE+32
              IF (F(I  ,J+1,K+1).LE.FISO) ICSE=ICSE+64
              IF (F(I+1,J+1,K+1).LE.FISO) ICSE=ICSE+128
              IF (ICSE.GT.0.AND.ICSE.LT.255) THEN
                IF (ICSE.LE.127) THEN
                  ITST=IACT(ICSE)
                ELSE
                  ICSE=255-ICSE
                  ITST=-IACT(ICSE)
                END IF
                DO 101 IPNT=1,8
                  BOXU(IPNT)=U(I+IOSU(IPER(IPNT,ICSE)))
                  BOXV(IPNT)=V(J+IOSV(IPER(IPNT,ICSE)))
                  BOXW(IPNT)=W(K+IOSW(IPER(IPNT,ICSE)))
                  BOXF(IPNT)=F(I+IOSU(IPER(IPNT,ICSE)),
     +                         J+IOSV(IPER(IPNT,ICSE)),
     +                         K+IOSW(IPER(IPNT,ICSE)))
  101           CONTINUE
                ISWP=(1-SIGN(1,ITST))/2
                NTRS=NTRI+1
                ITST=ABS(ITST)
                DO 102 L=LBEG(ITST),LEND(ITST)
                  IF (NTRI.LT.MTRI) THEN
                    NTRI=NTRI+1
                    RTRI(1,NTRI)=UINT(IP11(L),IP12(L))
                    RTRI(2,NTRI)=VINT(IP11(L),IP12(L))
                    RTRI(3,NTRI)=WINT(IP11(L),IP12(L))
                    RTRI(4,NTRI)=UINT(IP21(L),IP22(L))
                    RTRI(5,NTRI)=VINT(IP21(L),IP22(L))
                    RTRI(6,NTRI)=WINT(IP21(L),IP22(L))
                    RTRI(7,NTRI)=UINT(IP31(L),IP32(L))
                    RTRI(8,NTRI)=VINT(IP31(L),IP32(L))
                    RTRI(9,NTRI)=WINT(IP31(L),IP32(L))
                    RTRI(10,NTRI)=RRST
                  END IF
  102           CONTINUE
                IF (ISWP.NE.0) THEN
                  DO 103 ISWP=NTRS,NTRI
                    TMP1=RTRI(1,ISWP)
                    TMP2=RTRI(2,ISWP)
                    TMP3=RTRI(3,ISWP)
                    RTRI(1,ISWP)=RTRI(4,ISWP)
                    RTRI(2,ISWP)=RTRI(5,ISWP)
                    RTRI(3,ISWP)=RTRI(6,ISWP)
                    RTRI(4,ISWP)=TMP1
                    RTRI(5,ISWP)=TMP2
                    RTRI(6,ISWP)=TMP3
  103             CONTINUE
                END IF
              END IF
  104       CONTINUE
  105     CONTINUE
  106   CONTINUE
C
C Done.
C
        RETURN
C
      END
