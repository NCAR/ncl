C
C	$Id: mapconvrt.f,v 1.2 2000-07-11 23:07:39 haley Exp $
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
      PROGRAM CONVRT
        DIMENSION FLIM(4),PNTS(200)
        OPEN (UNIT=1,STATUS='OLD',FILE='ezmapdat',ERR=5)
        OPEN (UNIT=2,STATUS='NEW',FORM='UNFORMATTED',FILE='ezmapdata',
     +      ERR=6)
        REWIND 1
        REWIND 2
    1   READ (1,3,END=2) NPTS,IGID,IDLS,IDRS,(FLIM(I),I=1,4)
        IF (NPTS.GT.1) READ (1,4,END=2) (PNTS(I),I=1,NPTS)
        WRITE (2) NPTS,IGID,IDLS,IDRS,(FLIM(I),I=1,4),(PNTS(I),I=1,NPTS)
        GO TO 1
    2   STOP
    3   FORMAT (4I4,4F8.3)
    4   FORMAT (10F8.3)
    5   WRITE (6,*) 'Error in opening the file <ezmapdat>.'
        STOP
    6   WRITE(6,*) 'Error in creating the file <ezmapdata>.'
        STOP
      END
