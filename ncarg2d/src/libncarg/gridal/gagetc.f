C
C $Id: gagetc.f,v 1.4 2000-07-12 16:24:11 haley Exp $
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
      SUBROUTINE GAGETC (PNAM,CVAL)
C
        CHARACTER*(*) PNAM,CVAL
C
C The subroutine GAGETC may be used to get GRIDAL parameters which have
C values of type CHARACTER.
C
C Declare the common block containing character parameters.
C
        COMMON /GACHAR/ FNLX,FNLY
        CHARACTER*10    FNLX,FNLY
        SAVE   /GACHAR/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('GAGETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Get the parameter value desired.
C
        IF      (PNAM(1:3).EQ.'XLF'.OR.PNAM(1:3).EQ.'xlf') THEN
          CVAL=FNLX
        ELSE IF (PNAM(1:3).EQ.'YLF'.OR.PNAM(1:3).EQ.'ylf') THEN
          CVAL=FNLY
        ELSE
          CALL SETER ('GAGETC - UNRECOGNIZED PARAMETER NAME',2,1)
        END IF
C
C Done.
C
        RETURN
C
      END
