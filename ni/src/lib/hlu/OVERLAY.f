C
C $Id: OVERLAY.f,v 1.2 1994-06-03 20:34:32 dbrown Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1994			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
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
      subroutine nhlfaddtooverlay(id_base,id_plot,id_after,ierr)
	call nhlf_addtooverlay(id_base,id_plot,id_after,ierr)
      end
C
      subroutine nhlfremovefromoverlay(id_base,id_plot,irestore,ierr)
      	call nhlf_removefromoverlay(id_base,id_plot,irestore,ierr)
      end
C
      subroutine nhlfregisterannotation(id_overlay_base,
     +     id_annotation,ierr)
      	call nhlf_registerannotation(id_overlay_base,
     +     id_annotation,ierr)
      end
C
      subroutine nhlfunregisterannotation(id_overlay_base,
     +     id_annotation,ierr)
      	call nhlf_unregisterannotation(id_overlay_base,
     +     id_annotation,ierr)
      end

