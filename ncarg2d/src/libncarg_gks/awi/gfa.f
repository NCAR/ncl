C
C	$Id: gfa.f,v 1.7 2000-07-12 16:39:42 haley Exp $
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
      SUBROUTINE GFA(N,PX,PY)
C
C  FILL AREA
C
      INTEGER EFA
      PARAMETER (EFA=15)
C
C  Specify the maximum sized polygon that will be clipped to the NDC
C  viewport.
C
      PARAMETER (MAXCLP=250)
C
      include 'gkscom.h'
C
      REAL PX(N),PY(N)
C
C  Set up integer workspace for clipping call.
C
      DIMENSION IWKSP(IWDIM)
      EQUIVALENCE(IWKSP(1),RWKSP(1))
      DIMENSION WCLIPX(4),WCLIPY(4)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(5,EFA,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the number of points is valid.
C
      IF (.NOT.(N.GE.3)) THEN
        ERS = 1
        CALL GERHND(100,EFA,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set function code and put out the real arrays across the
C  workstation interface.  Flag conversion to NDC space.
C
      FCODE = 14
      CALL GZROI(0)
C
C  Set up clip rectangle for clipping world coordinates to the
C  NDC viewport if the polygons are small enough and clipping is on.
C
      IF (N .LE. MAXCLP .AND. GKSCLP.NE.0) THEN
        ICNT = CNT+1
        CALL GZN2WX(1,0.,WCLIPX(1))
        CALL GZN2WX(1,1.,WCLIPX(2))
        WCLIPX(3) = WCLIPX(2)
        WCLIPX(4) = WCLIPX(1)
        CALL GZN2WY(1,0.,WCLIPY(1))
        WCLIPY(2) = WCLIPY(1)
        CALL GZN2WY(1,1.,WCLIPY(3))
        WCLIPY(4) = WCLIPY(3)
        CALL GZCLPO (WCLIPX,WCLIPY,4,PX,PY,N,RWKSP,IWKSP,IWDIM,IERR)
        IF (IERR .NE. 0) GO TO 10
        RERR = 0
        RETURN
      ENDIF
C
C  Polygon is larger than the clip limit, or clip algorithm error
C  encountered, or clipping is off.
C
   10 CONTINUE
      NPTOT = N
      CALL GZPUTR(NPTOT,N,PX,PY,1,IER)
      RERR = IER
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EFA,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
