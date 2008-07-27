C
C	$Id: cstrlprt.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSTRLPRT (N,X,Y,Z,IFLAG,NROW,NT,LTRI,LOUT)
      INTEGER N, IFLAG, NROW, NT, LTRI(NROW,NT), LOUT
      DOUBLE PRECISION X(N), Y(N), Z(N)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/02/98
C
C   This subroutine prints the triangle list created by Sub-
C routine CSTRLIST and, optionally, the nodal coordinates
C (either latitude and longitude or Cartesian coordinates)
C on logical unit LOUT.  The numbers of boundary nodes,
C triangles, and arcs are also printed.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.
C           3 .LE. N .LE. 9999.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes if IFLAG = 0, or
C               (X and Y only) arrays of length N containing
C               longitude and latitude, respectively, if
C               IFLAG > 0, or unused dummy parameters if
C               IFLAG < 0.
C
C       IFLAG = Nodal coordinate option indicator:
C               IFLAG = 0 if X, Y, and Z (assumed to contain
C                         Cartesian coordinates) are to be
C                         printed (to 6 decimal places).
C               IFLAG > 0 if only X and Y (assumed to con-
C                         tain longitude and latitude) are
C                         to be printed (to 6 decimal
C                         places).
C               IFLAG < 0 if only the adjacency lists are to
C                         be printed.
C
C       NROW = Number of rows (entries per triangle) re-
C              served for the triangle list LTRI.  The value
C              must be 6 if only the vertex indexes and
C              neighboring triangle indexes are stored, or 9
C              if arc indexes are also stored.
C
C       NT = Number of triangles in the triangulation.
C            1 .LE. NT .LE. 9999.
C
C       LTRI = NROW by NT array whose J-th column contains
C              the vertex nodal indexes (first three rows),
C              neighboring triangle indexes (second three
C              rows), and, if NROW = 9, arc indexes (last
C              three rows) associated with triangle J for
C              J = 1,...,NT.
C
C       LOUT = Logical unit number for output.  If LOUT is
C              not in the range 0 to 99, output is written
C              to unit 6.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C   The triangle list and nodal coordinates (as specified by
C IFLAG) are written to unit LOUT.
C
C Modules required by CSTRLPRT:  None
C
C***********************************************************
C
      INTEGER I, K, LUN, NA, NB, NL, NLMAX, NMAX
      DATA    NMAX/9999/,  NLMAX/58/
C
C Local parameters:
C
C I =     DO-loop, nodal index, and row index for LTRI
C K =     DO-loop and triangle index
C LUN =   Logical unit number for output
C NA =    Number of triangulation arcs
C NB =    Number of boundary nodes
C NL =    Number of lines printed on the current page
C NLMAX = Maximum number of print lines per page (except
C           for the last page which may have two addi-
C           tional lines)
C NMAX =  Maximum value of N and NT (4-digit format)
C
      LUN = LOUT
      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
C
C Print a heading and test for invalid input.
C
      WRITE (LUN,100) N
      NL = 3
      IF (N .LT. 3  .OR.  N .GT. NMAX  .OR.
     .    (NROW .NE. 6  .AND.  NROW .NE. 9)  .OR.
     .    NT .LT. 1  .OR.  NT .GT. NMAX) THEN
C
C Print an error message and exit.
C
        WRITE (LUN,110) N, NROW, NT
        RETURN
      ENDIF
      IF (IFLAG .EQ. 0) THEN
C
C Print X, Y, and Z.
C
        WRITE (LUN,101)
        NL = 6
        DO 1 I = 1,N
          IF (NL .GE. NLMAX) THEN
            WRITE (LUN,108)
            NL = 0
          ENDIF
          WRITE (LUN,103) I, X(I), Y(I), Z(I)
          NL = NL + 1
    1     CONTINUE
      ELSEIF (IFLAG .GT. 0) THEN
C
C Print X (longitude) and Y (latitude).
C
        WRITE (LUN,102)
        NL = 6
        DO 2 I = 1,N
          IF (NL .GE. NLMAX) THEN
            WRITE (LUN,108)
            NL = 0
          ENDIF
          WRITE (LUN,104) I, X(I), Y(I)
          NL = NL + 1
    2     CONTINUE
      ENDIF
C
C Print the triangulation LTRI.
C
      IF (NL .GT. NLMAX/2) THEN
        WRITE (LUN,108)
        NL = 0
      ENDIF
      IF (NROW .EQ. 6) THEN
        WRITE (LUN,105)
      ELSE
        WRITE (LUN,106)
      ENDIF
      NL = NL + 5
      DO 3 K = 1,NT
        IF (NL .GE. NLMAX) THEN
          WRITE (LUN,108)
          NL = 0
        ENDIF
        WRITE (LUN,107) K, (LTRI(I,K), I = 1,NROW)
        NL = NL + 1
    3   CONTINUE
C
C Print NB, NA, and NT (boundary nodes, arcs, and
C   triangles).
C
      NB = 2*N - NT - 2
      IF (NB .LT. 3) THEN
        NB = 0
        NA = 3*N - 6
      ELSE
        NA = NT + N - 1
      ENDIF
      WRITE (LUN,109) NB, NA, NT
      RETURN
C
C Print formats:
C
  100 FORMAT (///18X,'STRIPACK (CSTRLIST) Output,  N = ',I4)
  101 FORMAT (//8X,'Node',10X,'X(Node)',10X,'Y(Node)',10X,
     .        'Z(Node)'//)
  102 FORMAT (//16X,'Node',8X,'Longitude',9X,'Latitude'//)
  103 FORMAT (8X,I4,3D17.6)
  104 FORMAT (16X,I4,2D17.6)
  105 FORMAT (//1X,'Triangle',8X,'Vertices',12X,'Neighbors'/
     .        4X,'KT',7X,'N1',5X,'N2',5X,'N3',4X,'KT1',4X,
     .        'KT2',4X,'KT3'/)
  106 FORMAT (//1X,'Triangle',8X,'Vertices',12X,'Neighbors',
     .        14X,'Arcs'/
     .        4X,'KT',7X,'N1',5X,'N2',5X,'N3',4X,'KT1',4X,
     .        'KT2',4X,'KT3',4X,'KA1',4X,'KA2',4X,'KA3'/)
  107 FORMAT (2X,I4,2X,6(3X,I4),3(2X,I5))
  108 FORMAT (///)
  109 FORMAT (/1X,'NB = ',I4,' Boundary Nodes',5X,
     .        'NA = ',I5,' Arcs',5X,'NT = ',I5,
     .        ' Triangles')
  110 FORMAT (//1X,10X,'*** Invalid Parameter:  N =',I5,
     .        ', NROW =',I5,', NT =',I5,' ***')
      END
