C
C $Id: RLGEN.f,v 1.3 1997-05-05 21:45:19 boote Exp $
C
C************************************************************************
C                                                                       *
C                            Copyright (C)  1994                        *
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
C                       needed for the Fortran programmer to manage
C                       ResLists.
C
C
C       Fortran.c
C
      subroutine nhlfrlcreate (listid,ltype)
        integer listid
        character*(*) ltype
        call nhlpfrlcreate(listid,ltype,len(ltype))
      end
      subroutine nhlfrldestroy (id)
        call nhlpfrldestroy (id)
      end
      subroutine nhlfrlclear (id)
        call nhlpfrlclear (id)
      end
      subroutine nhlfrlunset (id,name)
        character*(*) name
        call nhlpfrlunset (id,name,len(name))
      end
      subroutine nhlfrlisset (id,name,ival)
        character*(*) name
        call nhlpfrlisset (id,name,len(name),ival)
      end
