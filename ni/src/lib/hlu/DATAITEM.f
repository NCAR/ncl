C
C $Id: DATAITEM.f,v 1.2 1997-05-05 21:45:07 boote Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1995			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
C****************************************************************
C
C      File:            DATAITEM.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Feb 14 12:17:04 MST 1995
C
C      Description:     
C
      subroutine nhlfisdataitem(id,istat)

	integer id,istat
	call nhlpfisdataitem(id,istat)
      end
