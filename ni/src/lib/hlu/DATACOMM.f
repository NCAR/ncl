C
C $Id: DATACOMM.f,v 1.1 1994-05-12 23:50:53 boote Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1994			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
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
	call nhlf_updatedata(id_obj,ierr)
      end
      subroutine nhlfadddata(id_plot,res_name,id_data,ierr)
	character*(*) res_name
	integer id_plot,id_data,ierr
	call nhlf_adddata(id_plot,res_name,len(res_name),id_data,ierr)
      end
      subroutine nhlfremovedata(id_plot,res_name,id_data,ierr)
	character*(*) res_name
	integer id_plot,id_data,ierr
	call nhlf_removedata(id_plot,res_name,len(res_name),id_data,ierr)
      end
