C
C $Id: APP.f,v 1.1 1995-03-15 11:48:22 boote Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1995			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
C****************************************************************
C
C      File:            APP.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Mon Mar 13 23:23:17 MST 1995
C
C      Description:     
C
      subroutine nhlfisapp(id,istat)

	integer id,istat
	call nhl_fisapp(id,istat)
      end
