C
C $Id: APP.f,v 1.3 1997-05-05 21:44:57 boote Exp $
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
	call nhlpfisapp(id,istat)
      end

      subroutine nhlfappgetdefaultparentid(id)
	integer id
	call nhlpfappgetdefaultparentid(id)
      end
