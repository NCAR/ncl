C
C $Id: VIEW.f,v 1.1 1994-05-12 23:52:45 boote Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1994			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
C****************************************************************
C
C      File:            VIEW.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Fri Apr 15 16:55:34 MDT 1994
C
C      Description:     
C
      subroutine nhlfgetbb(ipid,top,bottom,left,right,ierr)
	integer ipid,ierr
	real top,bottom,left,right
	call nhlf_getbb(ipid,top,bottom,left,right,ierr)
      end
