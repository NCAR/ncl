      SUBROUTINE CSSTRI (N,X,Y,Z, NT,NTRI, IWK,WK,IER)
      INTEGER N, NT, NTRI(3,*), IWK(*), IER
      REAL    X(N), Y(N), Z(N), WK(*)
C
C***********************************************************
C
C                              Simplified STRIPACK interface
C                                                 Fred Clare
C                                                       NCAR
C                                                   03/04/99
C
C   This subroutine provides a simplified interface to
C STRIPACK triangulation, at the expense of requiring a
C little extra storage.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of distinct nodes.  (X(K),Y(K),
C               Z(K)) is referred to as node K, and K is re-
C               ferred to as a nodal index.  It is required
C               that X(K)**2 + Y(K)**2 + Z(K)**2 = 1 for all
C               K.  The first three nodes must not be col-
C               linear (lie on a common great circle).
C
C       NTRI =  A two-dimensional integer array dimensioned 
C               for 3 x NT where NT is the number of triangles
C               in the triangulation (NT is at most 2*N). 
C
C       IWK  =  An integer workspace of length 27*N.
C
C       RWK  =  A real workspace of length N.
C
C On output:
C
C       NT   = Number of triangles in the triangulation unless
C              IER .NE. 0, in which case NT = 0.  NT = 2N-NB-2
C              if NB .GE. 3 or 2N-4 if NB = 0, where NB is the
C              number of boundary nodes.
C
C       NTRI = The nodes for the triangles in the triangulation.
C              The nodes for the Jth triangle are NTRI(1,J),
C              NTRI(2,J) and NTRI(3,J) where node I references
C              the coordinate (X(I),Y(I),Z(I)).
C
C       IER  = Error indicator:
C              IER = 2  if the triangulation data structure
C                       (LIST,LPTR,LEND) is invalid.  Note,
C                       however, that these arrays are not
C                       completely tested for validity.
C              IER =  0 if no errors were encountered.
C              IER = -1 if N < 3 on input.
C              IER = -2 if the first three nodes are
C                       collinear.
C              IER = -3 if an error flag was returned by a
C                       call to CSSWAP in CSADDNOD.  This is an
C                       internal error and should be reported
C                       to the programmer.
C              IER =  L if nodes L and M coincide for some
C                       M > L.  The data structure represents
C                       a triangulation of nodes 1 to M-1 in
C                       this case.
C
C Modules required by CSSTRI:  CSTRMESH, CSTRLIST
C
C***********************************************************
C
C  Triangulate.
C
      CALL CSTRMESH(N,X,Y,Z, IWK(1),IWK(6*N+1),IWK(12*N+1),LNEW,
     +              IWK(13*N+1),IWK(14*N+1),WK(1),IER)
      IF (IER .NE. 0) RETURN
C
C  Determine the triangle nodes.
C
      CALL CSTRLIST(N,IWK(1),IWK(6*N+1),IWK(12*N+1),
     +              6,NT,IWK(15*N+1),IER)
      IF (IER .NE. 0) RETURN
C
C  Copy off the triangle nodes and return.
C
      DO 10 J=1,NT
        NTRI(1,J) = IWK(15*N+6*(J-1)+1) 
        NTRI(2,J) = IWK(15*N+6*(J-1)+2) 
        NTRI(3,J) = IWK(15*N+6*(J-1)+3) 
   10 CONTINUE
C
      RETURN
      END
