C
C $Id: OVERLAY.f,v 1.4 1995-03-21 22:36:55 dbrown Exp $
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
      subroutine nhlfaddannotation(id_overlay_plot,
     +     id_anno_view,id_anno,ierr)
      	call nhlf_addannotation(id_overlay_plot,
     +     id_anno_view,id_anno,ierr)
      end
C
      subroutine nhlfremoveannotation(id_overlay_plot,
     +     id_anno,ierr)
      	call nhlf_removeannotation(id_overlay_plot,
     +     id_anno,ierr)
      end
C
c$$$      subroutine nhlfgetannotationid(id_overlay_base,
c$$$     +     id_anno_view,id_anno,ierr)
c$$$      	call nhlf_getannotationid(id_overlay_base,
c$$$     +     id_anno_view,id_anno,ierr)
c$$$      end
C

