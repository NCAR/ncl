C
C $Id: nwtstr.f,v 1.5 2000-07-12 16:24:24 haley Exp $
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
C *************************************************************
C
      SUBROUTINE NWTSTR (PX,PY,CH)
C
C *************************************************************
C
C This subroutine is a routine which converts calls to
C GTX to calls to PCHIQU.  Direct calls to GTX produce
C distortions in the characters due to the current
C normalization transformation.  PCHIQU avoids this
C problem by using a uniform transformation.
C
      CHARACTER*(*) CH
      REAL VP(4),WN(4)
C
C Determine the centering option.
C
      CALL GQCNTN(IER,INT)
      CALL GQNT(INT,IER,WN,VP)
      CALL GQTXAL(IER,IHZ,IDUM)
C
        ICENT = IHZ-2
	IF (IHZ .EQ. 0) ICENT = -1
	CNTR = FLOAT(ICENT)
C
C Determine character height.
C
      CALL GQCHH (IER,CHARH)
C
	 YPORT  = VP(4) - VP(3)
	SIZE = CHARH * YPORT
C
C Determine character orientation.
C
      CALL GQCHUP(IER,XV,YV)
      ANGD = 57.29634*ATAN2(-XV,YV)
      IANG = ANGD + .01
      IF (IANG.LT. 0) IANG = IANG+360
      ANGD = FLOAT(IANG)
C
C Invoke PCHIQU (formerly PLCHHQ) of the Plotchar utility.
C
      CALL PCHIQU (PX,PY,CH,SIZE,ANGD,CNTR)
C
      RETURN
      END
