C
C	$Id: cscovsph.f,v 1.3 2000-07-13 02:49:08 haley Exp $
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
      SUBROUTINE CSCOVSPH (KK,N0, LIST,LPTR,LEND,LNEW )
      INTEGER KK, N0, LIST(*), LPTR(*), LEND(*), LNEW
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/17/96
C
C   This subroutine connects an exterior node KK to all
C boundary nodes of a triangulation of KK-1 points on the
C unit sphere, producing a triangulation that covers the
C sphere.  The data structure is updated with the addition
C of node KK, but no optimization is performed.  All boun-
C dary nodes must be visible from node KK.
C
C
C On input:
C
C       KK = Index of the node to be connected to the set of
C            all boundary nodes.  KK .GE. 4.
C
C       N0 = Index of a boundary node (in the range 1 to
C            KK-1).  N0 may be determined by Subroutine
C            CSTRFIND.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Triangulation data structure
C                             created by Subroutine CSTRMESH.
C                             Node N0 must be included in
C                             the triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node KK as the
C                             last entry.  The updated
C                             triangulation contains no
C                             boundary nodes.
C
C Module required by CSCOVSPH:  CSINSERT
C
C***********************************************************
C
      INTEGER K, LP, LSAV, NEXT, NST
C
C Local parameters:
C
C K =     Local copy of KK
C LP =    LIST pointer
C LSAV =  LIST pointer
C NEXT =  Boundary node visible from K
C NST =   Local copy of N0
C
      K = KK
      NST = N0
C
C Traverse the boundary in clockwise order, inserting K as
C   the first neighbor of each boundary node, and converting
C   the boundary node to an interior node.
C
      NEXT = NST
    1 LP = LEND(NEXT)
        CALL CSINSERT (K,LP, LIST,LPTR,LNEW )
        NEXT = -LIST(LP)
        LIST(LP) = NEXT
        IF (NEXT .NE. NST) GO TO 1
C
C Traverse the boundary again, adding each node to K's
C   adjacency list.
C
      LSAV = LNEW
    2 LP = LEND(NEXT)
        LIST(LNEW) = NEXT
        LPTR(LNEW) = LNEW + 1
        LNEW = LNEW + 1
        NEXT = LIST(LP)
        IF (NEXT .NE. NST) GO TO 2
C
      LPTR(LNEW-1) = LSAV
      LEND(K) = LNEW - 1
      RETURN
      END
