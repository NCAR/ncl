C
C $Id: RLGET.f,v 1.5 1997-05-05 21:45:20 boote Exp $
C
C****************************************************************
C                                                               *
C                       Copyright (C)  1994                     *
C       University Corporation for Atmospheric Research         *
C                       All Rights Reserved                     *
C                                                               *
C****************************************************************
C
C      File:            RLGET.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Thu Apr 7 14:16:21 MDT 1994
C
C      Description:     
C
      subroutine nhlfrlgetinteger(id,name,ival,ierr)
        integer id, ival,ierr
        character*(*) name
        call nhlpfrlgetinteger(id,name,len(name),ival,ierr)
      end
C
      subroutine nhlfrlgetfloat(id,name,fval,ierr)
        integer id,ierr
        real fval
        character*(*) name
        call nhlpfrlgetfloat(id,name,len(name),fval,ierr)
      end
C
      subroutine nhlfrlgetdouble(id,name,dval,ierr)
        integer id,ierr
        double precision dval
        character*(*) name
        call nhlpfrlgetdouble(id,name,len(name),dval,ierr)
      end
C
      subroutine nhlfrlgetstring(id,name,sval,ierr)
        integer id,ierr
        character*(*) name,sval
        call nhlpfrlgetstring(id,name,len(name),sval,len(sval),ierr)
      end
C
      subroutine nhlfrlgetmdintegerarray(id,name,iarr,inumdim,ilendim,
     % ierr)
        integer id,iarr(*),inumdim,ilendim,ierr
        character*(*) name
        call nhlpfrlgetmdintegerarray(id,name,len(name),iarr,inumdim,
     %  ilendim,ierr)
      end
C
      subroutine nhlfrlgetmdfloatarray(id,name,farr,inumdim,ilendim,
     %  ierr)
        integer id,inumdim,ilendim,ierr
        character*(*) name
        real farr(*)
        call nhlpfrlgetmdfloatarray(id,name,len(name),farr,inumdim,
     %  ilendim,ierr)
      end
C
      subroutine nhlfrlgetmddoublearray(id,name,darr,inumdim,ilendim,
     %  ierr)
        integer id,inumdim,ilendim,ierr
        character*(*) name
        double precision darr(*)
        call nhlpfrlgetmddoublearray(id,name,len(name),darr,inumdim,
     %  ilendim,ierr)
      end
C
      subroutine nhlfrlgetintegerarray(id,name,iarr,iarr_len,ierr)
        integer id,iarr_len,iarr(iarr_len),ierr
        character*(*) name
        call nhlpfrlgetintegerarray(id,name,len(name),iarr,iarr_len,
     % ierr)
      end
C
      subroutine nhlfrlgetfloatarray(id,name,farr,farr_len,ierr)
        integer id,farr_len,ierr
        character*(*) name
        real farr(farr_len)
        call nhlpfrlgetfloatarray(id,name,len(name),farr,farr_len,ierr)
      end
C
      subroutine nhlfrlgetdoublearray(id,name,darr,darr_len,ierr)
        integer id,darr_len,ierr
        character*(*) name
        real darr(darr_len)
        call nhlpfrlgetdoublearray(id,name,len(name),darr,darr_len,ierr)
      end
C
      subroutine nhlfrlgetstringarray(id,name,carr,carr_len,ierr)
        integer id,carr_len,ierr
        character*(*) name
        character*(*) carr(carr_len)
        call nhlpfrlgetstringarray(id,name,len(name),carr,len(carr(1)),
     %  carr_len,ierr)
      end
