C
C $Id: VIEWF.f,v 1.1 2001-10-09 00:18:37 haley Exp $
C
C****************************************************************
C                                                               *
C                       Copyright (C)  1994                     *
C       University Corporation for Atmospheric Research         *
C                       All Rights Reserved                     *
C                                                               *
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
        call nhlfpgetbb(ipid,top,bottom,left,right,ierr)
      end
      subroutine nhlfisview(id,istat)

        integer id,istat
        call nhlpfisview(id,istat)
      end
