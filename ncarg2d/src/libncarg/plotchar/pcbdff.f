C
C	$Id: pcbdff.f,v 1.5 2002-02-07 23:17:56 fred Exp $
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
      BLOCK DATA PCBDFF
C
C BLOCK DATA for filled fonts.
C
      include 'pcffme.doc'
      include 'pcffme.h'
      include 'pcffdx.doc'
      include 'pcffdx.h'
      include 'pcffsp.doc'
      include 'pcffsp.h'
C
C Initialize a single variable to avoid a loader error on
C some systems.  This blockdata subroutine is here as a
C dummy in case it should ever be needed in the future.
C
      DATA TYPFLG / 0 /
C
      END
