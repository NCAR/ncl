C
C $Id: RLGEN.f,v 1.2 1995-03-31 00:50:51 boote Exp $
C
C************************************************************************
C                                                                       *
C                            Copyright (C)  1994			*
C                 University Corporation for Atmospheric Research       *
C                            All Rights Reserved                        *
C                                                                       *
C************************************************************************
C
C      File:            RLGEN.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Thu Mar 24 15:58:56 MST 1994
C
C      Description:     This file contains the "generic" list functions
C			needed for the Fortran programmer to manage
C			ResLists.
C
C
C	Fortran.c
C
      subroutine nhlfrlcreate (listid,ltype)
	integer listid
	character*(*) ltype
	call nhl_frlcreate(listid,ltype,len(ltype))
      end
      subroutine nhlfrldestroy (id)
      	call nhl_frldestroy (id)
      end
      subroutine nhlfrlclear (id)
      	call nhl_frlclear (id)
      end
      subroutine nhlfrlunset (id,name)
	character*(*) name
      	call nhl_frlunset (id,name,len(name))
      end
      subroutine nhlfrlisset (id,name,ival)
	character*(*) name
      	call nhl_frlisset (id,name,len(name),ival)
      end
