C
C $Id: RLSET.f,v 1.6 1997-05-05 21:45:20 boote Exp $
C
C****************************************************************
C                                                               *
C                       Copyright (C)  1994                     *
C       University Corporation for Atmospheric Research         *
C                       All Rights Reserved                     *
C                                                               *
C****************************************************************
C
C      File:            RLSET.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Fri Mar 25 10:54:56 MST 1994
C
C      Description:     This file contains all the "set" subroutines for
C                       the Fortran RL interface to the hlu's.
C
      subroutine nhlfrlsetinteger(id,name,ival,ierr)
        integer id, ival,ierr
        character*(*) name
        call nhlpfrlsetinteger(id,name,len(name),ival,ierr)
      end
C
      subroutine nhlfrlsetfloat(id,name,fval,ierr)
        integer id,ierr
        real fval
        character*(*) name
        call nhlpfrlsetfloat(id,name,len(name),fval,ierr)
      end
C
      subroutine nhlfrlsetdouble(id,name,dval,ierr)
        integer id,ierr
        double precision dval
        character*(*) name
        call nhlpfrlsetdouble(id,name,len(name),dval,ierr)
      end
C
      subroutine nhlfrlsetstring(id,name,sval,ierr)
        integer id,ierr
        character*(*) name,sval
        call nhlpfrlsetstring(id,name,len(name),sval,len(sval),ierr)
      end
C
      subroutine nhlfrlsetmdintegerarray(id,name,iarr,inumdim,ilendim,
     % ierr)
        integer id,iarr(*),inumdim,ilendim,ierr
        character*(*) name
        call nhlpfrlsetmdintegerarray(id,name,len(name),iarr,inumdim,
     %  ilendim,ierr)
      end
C
      subroutine nhlfrlsetmdfloatarray(id,name,farr,inumdim,ilendim,
     %  ierr)
        integer id,inumdim,ilendim,ierr
        character*(*) name
        real farr(*)
        call nhlpfrlsetmdfloatarray(id,name,len(name),farr,inumdim,
     %  ilendim,ierr)
      end
C
      subroutine nhlfrlsetmddoublearray(id,name,darr,inumdim,ilendim,
     %  ierr)
        integer id,inumdim,ilendim,ierr
        character*(*) name
        double precision darr(*)
        call nhlpfrlsetmddoublearray(id,name,len(name),darr,inumdim,
     %  ilendim,ierr)
      end
C
      subroutine nhlfrlsetintegerarray(id,name,iarr,iarr_len,ierr)
        integer id,iarr_len,iarr(iarr_len),ierr
        character*(*) name
        call nhlpfrlsetintegerarray(id,name,len(name),iarr,iarr_len,
     % ierr)
      end
C
      subroutine nhlfrlsetfloatarray(id,name,farr,farr_len,ierr)
        integer id,farr_len,ierr
        character*(*) name
        real farr(farr_len)
        call nhlpfrlsetfloatarray(id,name,len(name),farr,farr_len,ierr)
      end
C
      subroutine nhlfrlsetdoublearray(id,name,darr,darr_len,ierr)
        integer id,darr_len,ierr
        character*(*) name
        double precision darr(darr_len)
        call nhlpfrlsetdoublearray(id,name,len(name),darr,darr_len,ierr)
      end
C
      subroutine nhlfrlsetstringarray(id,name,carr,carr_len,ierr)
        integer id,carr_len,ierr
        character*(*) name
        character*(*) carr(carr_len)
        call nhlpfrlsetstringarray(id,name,len(name),carr,carr_len,
     %  len(carr(1)),ierr)
      end
