C
C	$Id: gsvp.f,v 1.4 2000-07-12 16:40:00 haley Exp $
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
      SUBROUTINE GSVP(TNR,XMIN,XMAX,YMIN,YMAX)
C
C  SET VIEWPORT
C
      INTEGER ESVP
      PARAMETER (ESVP=50)
C
      include 'gkscom.h'
C
      INTEGER TNR
      REAL XMIN,XMAX,YMIN,YMAX
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESVP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the normalization transformation number is valid.
C
      IF (TNR.LT.1 .OR. TNR.GT.MNT) THEN
        ERS = 1
        CALL GERHND(50,ESVP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that the rectangle definition is valid.
C
      IF (XMAX.LE.XMIN .OR. YMAX.LE.YMIN) THEN
        ERS = 1
        CALL GERHND(51,ESVP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that viewport lies in NDC space.
C
      IF (XMIN.LT.0. .OR. XMAX.GT.1. .OR.
     +    YMIN.LT.0. .OR. YMAX.GT.1.) THEN
        ERS = 1
        CALL GERHND(52,ESVP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the normalization transformation viewport in the GKS state list.
C
      INR = TNR+1
      NTVP(INR,1) = XMIN
      NTVP(INR,2) = XMAX
      NTVP(INR,3) = YMIN
      NTVP(INR,4) = YMAX
C
C  Reestablish character height and up vector, and
C  pattern size and reference point, and clipping rectangle
C  if the normalization number TNR is the current one.
C
      CALL GQCNTN(IER,ICUR)
      IF (TNR .EQ. ICUR) THEN
        CALL GSCHH(CCHH)
        CALL GSCHUP(CCHUP(1),CCHUP(2))
C
C  Pattern size and pattern reference point currently not implemented.
C
C       CALL GSPA(CPA(1),CPA(2))
C       CALL GSPARF(CPARF(1),CPARF(2))
        CALL GSCLIP(CCLIP)
      ENDIF
C
      RETURN
      END
