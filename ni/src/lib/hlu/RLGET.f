C
C $Id: RLGET.f,v 1.2 1994-06-02 19:23:20 haley Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1994			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
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
      subroutine nhlfrlgetint(id,name,ival,ierr)
	integer id, ival,ierr
	character*(*) name
      	call nhl_frlgetint(id,name,len(name),ival,ierr)
      end
C
      subroutine nhlfrlgetfloat(id,name,fval,ierr)
	integer id,ierr
	real fval
	character*(*) name
      	call nhl_frlgetfloat(id,name,len(name),fval,ierr)
      end
C
      subroutine nhlfrlgetstring(id,name,sval,ierr)
	integer id,ierr
	character*(*) name,sval
      	call nhl_frlgetstring(id,name,len(name),sval,len(sval),ierr)
      end
C
      subroutine nhlfrlgetmdintarray(id,name,iarr,inumdim,ilendim,ierr)
	integer id,iarr(*),inumdim,ilendim,ierr
	character*(*) name
	call nhl_frlgetmdintarray(id,name,len(name),iarr,inumdim,
     %	ilendim,ierr)
      end
C
      subroutine nhlfrlgetmdfloatarray(id,name,farr,inumdim,ilendim,
     %	ierr)
	integer id,inumdim,ilendim,ierr
	character*(*) name
	real farr(*)
	call nhl_frlgetmdfloatarray(id,name,len(name),farr,inumdim,
     %	ilendim,ierr)
      end
C
      subroutine nhlfrlgetintarray(id,name,iarr,iarr_len,ierr)
	integer id,iarr_len,iarr(iarr_len),ierr
	character*(*) name
	call nhl_frlgetintarray(id,name,len(name),iarr,iarr_len,ierr)
      end
C
      subroutine nhlfrlgetfloatarray(id,name,farr,farr_len,ierr)
	integer id,farr_len,ierr
	character*(*) name
	real farr(farr_len)
	call nhl_frlgetfloatarray(id,name,len(name),farr,farr_len,ierr)
      end
C
      subroutine nhlfrlgetstringarray(id,name,carr,carr_len,ierr)
	integer id,carr_len,ierr
	character*(*) name
	character*(*) carr(carr_len)
	call nhl_frlgetstringarray(id,name,len(name),carr,len(carr(1)),
     %	carr_len,ierr)
      end
