C
C $Id: slfrme.f,v 1.2 2000-07-12 16:25:58 haley Exp $
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
      SUBROUTINE SLFRME
C
C The object of calling this routine is really just to advance the
C frame.  The calls to PLOTIF are to work around a GKS/translator
C problem: the background color doesn't get set properly for a totally
C blank frame, which it's easy to have in STITLE.
C
        CALL PLOTIF (0.,0.,0)
        IF (ICFELL('SLFRME',1).NE.0) RETURN
        CALL PLOTIF (0.,0.,1)
        IF (ICFELL('SLFRME',2).NE.0) RETURN
C
        CALL FRAME
        IF (ICFELL('SLFRME',3).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
