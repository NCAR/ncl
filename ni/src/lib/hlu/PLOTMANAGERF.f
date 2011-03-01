C
C $Id: PLOTMANAGERF.f,v 1.1 2001-10-09 00:18:35 haley Exp $
C
C****************************************************************
C                                                               *
C                       Copyright (C)  1994                     *
C       University Corporation for Atmospheric Research         *
C                       All Rights Reserved                     *
C                                                               *
C****************************************************************
C
C      File:            OVERLAY.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Fri Apr 15 16:53:27 MDT 1994
C
C      Description:     
C
      subroutine nhlfaddoverlay(id_base,id_plot,id_after,ierr)
        call nhlfpaddoverlay(id_base,id_plot,id_after,ierr)
      end
C
      subroutine nhlfremoveoverlay(id_base,id_plot,irestore,ierr)
        call nhlfpremoveoverlay(id_base,id_plot,irestore,ierr)
      end
C
      subroutine nhlfaddannotation(plot_id,view_id,anno_manager_id)
        call nhlfpaddannotation(plot_id,view_id,anno_manager_id)
      end
C
      subroutine nhlfremoveannotation(plot_id,anno_manager_id,ierr)
        call nhlfpremoveannotation(plot_id,anno_manager_id,ierr)
      end
C


