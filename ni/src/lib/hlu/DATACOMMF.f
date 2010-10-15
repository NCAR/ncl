C
C $Id: DATACOMMF.f,v 1.1 2001-10-09 00:18:34 haley Exp $
C
C****************************************************************
C                                                               *
C                       Copyright (C)  1994                     *
C       University Corporation for Atmospheric Research         *
C                       All Rights Reserved                     *
C                                                               *
C****************************************************************
C
C      File:            DATACOMM.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Fri Apr 15 16:51:43 MDT 1994
C
C      Description:     
C
      subroutine nhlfupdatedata(id_obj,ierr)
        call nhlfpupdatedata(id_obj,ierr)
      end
      subroutine nhlfadddata(id_plot,res_name,id_data,ierr)
        character*(*) res_name
        integer id_plot,id_data,ierr
        call nhlfpadddata(id_plot,res_name,len(res_name),id_data,ierr)
      end
      subroutine nhlfremovedata(id_plot,res_name,id_data,ierr)
        character*(*) res_name
        integer id_plot,id_data,ierr
        call nhlfpremovedata(id_plot,res_name,len(res_name),
     +                       id_data,ierr)
      end
      subroutine nhlfisdatacomm(id,istat)

        integer id,istat
        call nhlpfisdatacomm(id,istat)
      end
      subroutine nhlfisdataspec(id,istat)

        integer id,istat
        call nhlpfisdataspec(id,istat)
      end
