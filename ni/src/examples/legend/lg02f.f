CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           lg02f.f
C
C      Author:         Ed Stautler
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 13 18:31:18 MDT 1995
C
C      Description:    Demonstrates a Legend of 5 markers.
C
      external nhlfhlulayerclass
      external nhlfreslistlayerclass
      external nhlfapplayerclass
      external nhlflegendlayerclass
      external nhlfxworkstationlayerclass
        
      integer appid, wid, pid
      integer rlist, ierr

      character*8 labels(5)
      integer colors(5)
      integer item_ind(5)
      real mkthik(5)
  
C
C Initialize data values
C
      data labels / 'Marker_0', 'Marker_1', 'Marker_2', 
     1     'Marker_3', 'Marker_4' /

      data colors / 11, 21, 31, 41, 51 /

      data mkthik / 2.0, 3.0, 4.0, 5.0, 6.0 /

      data item_ind / 2, 3, 4, 5, 6 /
C
C Initialize the high level utility library
C
      call nhlfinitialize
C
C Create an application context.  By default the application looks
C for a resource file in the working directory.
C In this example the resource file supplies the plot title only.
C
      call nhlfrlcreate(rlist,'SETRL')
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appDefaultParent','True',ierr)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfcreate(appid,'lg02',nhlfapplayerclass,0,rlist,ierr)

C
C Create an XWorkstation object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'wkPause',1,ierr)
      call nhlfcreate(wid,'lg02Work',nhlfxworkstationlayerclass,0,
     1      rlist,ierr)
C
C Specify the viewport extent of the object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'vpXF',0.,ierr)
      call nhlfrlsetfloat(rlist,'vpYF',1.,ierr)
      call nhlfrlsetfloat(rlist,'vpWidthF',1.,ierr)
      call nhlfrlsetfloat(rlist,'vpHeightF',1.,ierr)
C
C Specify the type of markers for the legend.
C
      call nhlfrlsetinteger(rlist,'lgItemCount',5,ierr)
      call nhlfrlsetfloat(rlist,'lgLabelFontHeightF',.03,ierr)
      call nhlfrlsetstringarray(rlist,'lgLabelStrings',labels,
     1      5,ierr)
      call nhlfrlsetinteger(rlist,'lgMonoItemType',1,ierr)
      call nhlfrlsetinteger(rlist,'lgItemType',1,ierr)
      call nhlfrlsetintegerarray(rlist,'lgMarkerColors',colors,
     1      5,ierr)
      call nhlfrlsetintegerarray(rlist,'lgMarkerIndexes',item_ind,
     1      5,ierr)
      call nhlfrlsetinteger(rlist,'lgMonoMarkerThickness',0,ierr)
      call nhlfrlsetfloatarray(rlist,'lgMarkerThicknesses',mkthik,
     1      5,ierr)
      call nhlfrlsetfloat(rlist,'lgMarkerSizeF',.05,ierr)
      call nhlfcreate(pid,'Legend',nhlflegendlayerclass,wid,rlist,
     1      ierr)

      call nhlfdraw(pid,ierr)
      call nhlfframe(wid,ierr)
      call nhlfdestroy(pid,ierr)
      call nhlfdestroy(wid,ierr)
      call nhlfdestroy(appid,ierr)
      call nhlfclose

      stop
      end
