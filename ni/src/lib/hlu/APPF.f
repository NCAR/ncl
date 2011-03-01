C
C $Id: APPF.f,v 1.1 2001-10-09 00:18:33 haley Exp $
C
C****************************************************************
C                                                               *
C                       Copyright (C)  1995                     *
C       University Corporation for Atmospheric Research         *
C                       All Rights Reserved                     *
C                                                               *
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
