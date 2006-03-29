C
C	$Id: gpl.f,v 1.10 2006-03-29 23:56:18 fred Exp $
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
      SUBROUTINE GPL(N,PX,PY)
C
C  POLYLINE
C
      INTEGER EPL
      PARAMETER (EPL=12)
C
C  Specify the maximum sized polygon that will be clipped to the NDC
C  viewport.
C
      PARAMETER (MAXCLP=250)
C
      include 'gkscom.h'
C
      INTEGER N
      REAL PX(N),PY(N)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(5,EPL,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the number of points is valid.
C
      IF (.NOT.(N.GE.2)) THEN
        ERS = 1
        CALL GERHND(100,EPL,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set function code and put out the real arrays across the
C  workstation interface.  Flag conversion to NDC space (no
C  conversion is necessary for transformation 0).
C
      FCODE = 11
      CALL GZROI(0)
C
C  Clip the polyline to the NDC viewport if the polylines are small 
C  enough and clipping is on.
C
      IF (N .LE. MAXCLP .AND. GKSCLP.NE.0) THEN
        CALL GZN2WX(1,0.,XLFT)
        CALL GZN2WX(1,1.,XRIT)
        CALL GZN2WY(1,0.,YBOT)
        CALL GZN2WY(1,1.,YTOP)
        CALL GZCLLI (XLFT,XRIT,YBOT,YTOP,PX,PY,N,RWKSP,IWDIM,IERR)
C
C  If error, abandon any clipping and just put the line out.
C
        IF (IERR .NE. 0) GO TO 10
        RERR = 0
        RETURN
      ENDIF
C
   10 CONTINUE
C
      NPTOT = N
      CALL GZPUTR(NPTOT,N,PX,PY,MIN(CNT,1),IER)
      RERR = IER
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EPL,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
