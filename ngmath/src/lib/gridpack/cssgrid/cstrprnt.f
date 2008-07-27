C
C	$Id: cstrprnt.f,v 1.5 2008-07-27 03:10:09 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSTRPRNT (N,X,Y,Z,IFLAG,LIST,LPTR,LEND,LOUT)
      INTEGER N, IFLAG, LIST(*), LPTR(*), LEND(N), LOUT
      DOUBLE PRECISION X(N), Y(N), Z(N)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/25/98
C
C   This subroutine prints the triangulation adjacency lists
C created by Subroutine CSTRMESH and, optionally, the nodal
C coordinates (either latitude and longitude or Cartesian
C coordinates) on logical unit LOUT.  The list of neighbors
C of a boundary node is followed by index 0.  The numbers of
C boundary nodes, triangles, and arcs are also printed.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3
C           and N .LE. 9999.
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
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        CSTRMESH.
C
C       LOUT = Logical unit for output.  If LOUT is not in
C              the range 0 to 99, output is written to
C              logical unit 6.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C   The adjacency lists and nodal coordinates (as specified
C by IFLAG) are written to unit LOUT.
C
C Modules required by CSTRPRNT:  None
C
C***********************************************************
C
      INTEGER I, INC, K, LP, LPL, LUN, NA, NABOR(400), NB,
     .        ND, NL, NLMAX, NMAX, NODE, NN, NT
      DATA  NMAX/9999/,  NLMAX/58/
C
C Local parameters:
C
C I =     NABOR index (1 to K)
C INC =   Increment for NL associated with an adjacency list
C K =     Counter and number of neighbors of NODE
C LP =    LIST pointer of a neighbor of NODE
C LPL =   Pointer to the last neighbor of NODE
C LUN =   Logical unit for output (copy of LOUT)
C NA =    Number of arcs in the triangulation
C NABOR = Array containing the adjacency list associated
C           with NODE, with zero appended if NODE is a
C           boundary node
C NB =    Number of boundary nodes encountered
C ND =    Index of a neighbor of NODE (or negative index)
C NL =    Number of lines that have been printed on the
C           current page
C NLMAX = Maximum number of print lines per page (except
C           for the last page which may have two addi-
C           tional lines)
C NMAX =  Upper bound on N (allows 4-digit indexes)
C NODE =  Index of a node and DO-loop index (1 to N)
C NN =    Local copy of N
C NT =    Number of triangles in the triangulation
C
      NN = N
      LUN = LOUT
      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
C
C Print a heading and test the range of N.
C
      WRITE (LUN,100) NN
      IF (NN .LT. 3  .OR.  NN .GT. NMAX) THEN
C
C N is outside its valid range.
C
        WRITE (LUN,110)
        RETURN
      ENDIF
C
C Initialize NL (the number of lines printed on the current
C   page) and NB (the number of boundary nodes encountered).
C
      NL = 6
      NB = 0
      IF (IFLAG .LT. 0) THEN
C
C Print LIST only.  K is the number of neighbors of NODE
C   that have been stored in NABOR.
C
        WRITE (LUN,101)
        DO 2 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
C
    1     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 1
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.  Correct the sign of the last
C     neighbor, add 0 to the end of the list, and increment
C     NB.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print the list of neighbors.
C
          INC = (K-1)/14 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,108)
            NL = INC
          ENDIF
          WRITE (LUN,104) NODE, (NABOR(I), I = 1,K)
          IF (K .NE. 14) WRITE (LUN,107)
    2     CONTINUE
      ELSEIF (IFLAG .GT. 0) THEN
C
C Print X (longitude), Y (latitude), and LIST.
C
        WRITE (LUN,102)
        DO 4 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
C
    3     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 3
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print X, Y, and NABOR.
C
          INC = (K-1)/8 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,108)
            NL = INC
          ENDIF
          WRITE (LUN,105) NODE, X(NODE), Y(NODE),
     .                    (NABOR(I), I = 1,K)
          IF (K .NE. 8) WRITE (LUN,107)
    4     CONTINUE
      ELSE
C
C Print X, Y, Z, and LIST.
C
        WRITE (LUN,103)
        DO 6 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
C
    5     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 5
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print X, Y, Z, and NABOR.
C
          INC = (K-1)/5 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,108)
            NL = INC
          ENDIF
          WRITE (LUN,106) NODE, X(NODE), Y(NODE),
     .                    Z(NODE), (NABOR(I), I = 1,K)
          IF (K .NE. 5) WRITE (LUN,107)
    6     CONTINUE
      ENDIF
C
C Print NB, NA, and NT (boundary nodes, arcs, and
C   triangles).
C
      IF (NB .NE. 0) THEN
        NA = 3*NN - NB - 3
        NT = 2*NN - NB - 2
      ELSE
        NA = 3*NN - 6
        NT = 2*NN - 4
      ENDIF
      WRITE (LUN,109) NB, NA, NT
      RETURN
C
C Print formats:
C
  100 FORMAT (///15X,'STRIPACK Triangulation Data ',
     .        'Structure,  N = ',I5//)
  101 FORMAT (1X,'Node',31X,'Neighbors of Node'//)
  102 FORMAT (1X,'Node',5X,'Longitude',6X,'Latitude',
     .        18X,'Neighbors of Node'//)
  103 FORMAT (1X,'Node',5X,'X(Node)',8X,'Y(Node)',8X,
     .        'Z(Node)',11X,'Neighbors of Node'//)
  104 FORMAT (1X,I4,4X,14I5/(1X,8X,14I5))
  105 FORMAT (1X,I4,2D15.6,4X,8I5/(1X,38X,8I5))
  106 FORMAT (1X,I4,3D15.6,4X,5I5/(1X,53X,5I5))
  107 FORMAT (1X)
  108 FORMAT (///)
  109 FORMAT (/1X,'NB = ',I4,' Boundary Nodes',5X,
     .        'NA = ',I5,' Arcs',5X,'NT = ',I5,
     .        ' Triangles')
  110 FORMAT (1X,10X,'*** N is outside its valid',
     .        ' range ***')
      END
