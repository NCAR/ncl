C
C $Id: RLSET.f,v 1.4 1994-12-29 21:45:35 haley Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1994			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
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
C			the Fortran RL interface to the hlu's.
C
      subroutine nhlfrlsetinteger(id,name,ival,ierr)
	integer id, ival,ierr
	character*(*) name
      	call nhl_frlsetinteger(id,name,len(name),ival,ierr)
      end
C
      subroutine nhlfrlsetfloat(id,name,fval,ierr)
	integer id,ierr
	real fval
	character*(*) name
      	call nhl_frlsetfloat(id,name,len(name),fval,ierr)
      end
C
      subroutine nhlfrlsetstring(id,name,sval,ierr)
	integer id,ierr
	character*(*) name,sval
      	call nhl_frlsetstring(id,name,len(name),sval,len(sval),ierr)
      end
C
      subroutine nhlfrlsetmdintegerarray(id,name,iarr,inumdim,ilendim,
     % ierr)
	integer id,iarr(*),inumdim,ilendim,ierr
	character*(*) name
	call nhl_frlsetmdintegerarray(id,name,len(name),iarr,inumdim,
     %	ilendim,ierr)
      end
C
      subroutine nhlfrlsetmdfloatarray(id,name,farr,inumdim,ilendim,
     %	ierr)
	integer id,inumdim,ilendim,ierr
	character*(*) name
	real farr(*)
	call nhl_frlsetmdfloatarray(id,name,len(name),farr,inumdim,
     %	ilendim,ierr)
      end
C
      subroutine nhlfrlsetintegerarray(id,name,iarr,iarr_len,ierr)
	integer id,iarr_len,iarr(iarr_len),ierr
	character*(*) name
	call nhl_frlsetintegerarray(id,name,len(name),iarr,iarr_len,
     % ierr)
      end
C
      subroutine nhlfrlsetfloatarray(id,name,farr,farr_len,ierr)
	integer id,farr_len,ierr
	character*(*) name
	real farr(farr_len)
	call nhl_frlsetfloatarray(id,name,len(name),farr,farr_len,ierr)
      end
C
      subroutine nhlfrlsetstringarray(id,name,carr,carr_len,ierr)
	integer id,carr_len,ierr
	character*(*) name
	character*(*) carr(carr_len)
	call nhl_frlsetstringarray(id,name,len(name),carr,carr_len,
     %  len(carr(1)),ierr)
      end
