C
C $Id: mdrgdi.f,v 1.1 2001-08-16 23:09:37 kennison Exp $
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
      SUBROUTINE MDRGDI (DINM)
C
C This is a user-replaceable routine that returns the name of the
C directory in which the RANGS/GSHHS data files have been placed.
C
        CHARACTER*(*) DINM
C
C A user version of this routine should have the following statement
C commented out or removed:
C
        CALL SETER ('MDRGDI - REPLACE ME - RETURN NAME OF DIRECTORY CONT
     +AINING DATA',1,1)
C
C If the data files are in the directory from which you're running,
C uncomment the following statement:
C
        DINM='.'
C
C If the data files are in the directory "/tmp/user-name", uncomment and
C modify the following statement appropriately:
C
C       DINM='/tmp/user-name'
C
C Done.
C
        RETURN
C
      END
