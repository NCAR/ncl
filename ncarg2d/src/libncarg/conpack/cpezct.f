C
C $Id: cpezct.f,v 1.6 2000-07-12 16:22:27 haley Exp $
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
      SUBROUTINE CPEZCT (ZDAT,MZDT,NZDT)
C
      DIMENSION ZDAT(MZDT,NZDT)
C
C This routine simulates the old routine EZCNTR.
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPEZCT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Call CPCNRC to do the job.
C
      CALL CPCNRC (ZDAT,MZDT,MZDT,NZDT,0.,0.,0.,0,0,-682)
      IF (ICFELL('CPEZCT',2).NE.0) RETURN
C
C Advance the frame.
C
      CALL FRAME
      IF (ICFELL('CPEZCT',3).NE.0) RETURN
C
      RETURN
C
      END
