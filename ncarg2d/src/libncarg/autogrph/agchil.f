C
C $Id: agchil.f,v 1.3 2000-07-12 16:21:55 haley Exp $
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
      SUBROUTINE AGCHIL (IFLG,LBNM,LNNO)
C
      CHARACTER*(*) LBNM
C
C The routine AGCHIL is called by AGLBLS just before and just after each
C informational-label line of text is drawn.  The default version does
C nothing.  A user may supply a version to change the appearance of the
C text lines.  The arguments are as follows:
C
C - IFLG is zero if a text line is about to be drawn, non-zero if one
C   has just been drawn.
C
C - LBNM is the name of the label containing the line in question.
C
C - LNNO is the number of the line.
C
C Done.
C
      RETURN
C
      END
