;
;      $Id: nm20n.ncl,v 1.9 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  2000                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm20n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Thu Jun  3 15:16:01 MDT 1999
;
;   Description:  This program illustrates the use of the triangulation
;                 and Voronoi diagram capabilities of the cssgrid package.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

procedure draw_box(wks:graphic, xll[1]:float, yll[1]:float, xur[1]:float,
                   yur[1]:float, cindex[1]:integer, resources:logical)
;
;  Draw a box specified by the first four arguments and fill
;  it with the color whose index is given by cindex.  The arguments
;  are assumed to be in NDC space.
;
local xcoord, ycoord, cr
begin
  xcoord = new(5,float)
  ycoord = new(5,float)
  xcoord(0) = xll
  ycoord(0) = yll 
  xcoord(1) = xur
  ycoord(1) = yll 
  xcoord(2) = xur
  ycoord(2) = yur
  xcoord(3) = xll
  ycoord(3) = yur
  xcoord(4) = xll
  ycoord(4) = yll 
  cr = resources@gsFillColor
  resources@gsFillColor = cindex
  gsn_polygon_ndc(wks, xcoord, ycoord, resources)
  resources@gsFillColor = cr
  delete(xcoord)
  delete(ycoord)
end

;
;  Main program
;
begin
 
;
;  Number of points to use for drawing arcs on the globe.
;
  NARC = 50

;
;  Input dataset on the globe (latitudes and longitudes in degrees).
;  These data points do not cover the globe, but rather are confined
;  to the nothern hemisphere and are enclosed by a boundary.
; 
  plat = (/  70.,  70., 70., 85., 60., 60., 65./)
  plon = (/-160., -70.,  0., 20., 50., 80.,140./)

;
;  Determine tha size of the input arrays; create arrays for
;  storing the Cartesian coordinate equivalents to the lat/lon input;
;  calculate the Cartesian coordinates.
;  
  fsize = dimsizes(plat)

;
;  Create the Delaunay triangulation; "triangles" is returned as
;  a two dimensional array dimensioned for fsize x 3.
;
  triangles = csstri(plat,plon)
  tri_size = dimsizes(triangles)
  num_triangles = tri_size(0)

;
;  Get the circumcenters for the Delaunay triangles and store
;  them in arrays xc, yc, and zc.  "nca" is the actual number 
;  of circles found.
;
  rlat = new(2*fsize,float)
  rlon = new(2*fsize,float)
  rc = new(2*fsize,float)
  nca = new(1,integer)
  numv = new(1,integer)
  nv = new(fsize,integer)
  csvoro(plat,plon,0,1,rlat,rlon,rc,nca,numv,nv)

;
;  Draw a plot of the triangulation, the circumcircles, and the
;  Voronoi polygons.
;

;
;  Define a color map.
;
  cmap = (/                      \
            (/ 0., 0., 0. /),    \
            (/ 1., 1., 1. /),    \
            (/ 0., 1., 1. /),    \
            (/ 1., 1., 0. /),    \
            (/ 1., 0., 0. /),    \
            (/ 0., 1., 0. /),    \
            (/ 0., .8, 0. /)     \
         /)
;
; Create a workstation.
;
  wks_type = "ncgm"
  wks = gsn_open_wks(wks_type,"nm20n")

  gsn_define_colormap(wks,cmap)

;
;  Define some resources and draw a globe as a background for 
;  the plot.
;
  map_resources = True
  map_resources@gsnFrame = False
  map_resources@mpGeophysicalLineColor = 6
  map_resources@mpGridLineColor = 0
  map_resources@mpLimbLineColor = 6
  map_resources@mpCenterLatF = 72.5
  map_resources@mpCenterLonF = 127.5
  map_resources@mpOutlineBoundarySets = "National"
  map_resources@mpNationalLineColor = 6
  map_resources@vpXF = 0.175
  map_resources@vpYF = 0.825
  map_resources@vpWidthF = 0.8
  map_resources@vpHeightF = 0.8
  map_resources@mpGreatCircleLinesOn = True
  map = gsn_map(wks,"Satellite",map_resources)

;
;  Plot the circumcircles.  Only those circles going through
;  original data values are plotted and not those going through
;  the pseudo points.
;
  arclat = new(NARC, float)
  arclon = new(NARC, float)

  gsres = True
  gsres@gsLineThicknessF = 2.
  gsres@gsLineColor = 4
  do i=4,9
    nggcog(rlat(i), rlon(i), rc(i), arclat, arclon)
    gsn_polyline(wks,map,arclon,arclat,gsres)
  end do

;
;  Draw the Voronoi polygons.
;
  gsres@gsLineColor = 3
  rlatn = new(2,float)
  rlonn = new(2,float)
  do i=0,fsize-1
    csvoro(plat,plon,i,0,rlat,rlon,rc,nca,numv,nv)
    do j=1,numv-1
      rlatn(0) = rlat(nv(j-1))
      rlonn(0) = rlon(nv(j-1))
      rlatn(1) = rlat(nv(j))
      rlonn(1) = rlon(nv(j))
      gsn_polyline(wks,map,rlonn,rlatn,gsres)
    end do
  end do

;
;  Draw the triangles.
;
  qlat = new(4,float)
  qlon = new(4,float)
  gsres@gsLineColor = 2
  do np=0,num_triangles-1
    qlat(0) = plat(triangles(np,0))
    qlon(0) = plon(triangles(np,0))
    qlat(1) = plat(triangles(np,1))
    qlon(1) = plon(triangles(np,1))
    qlat(2) = plat(triangles(np,2))
    qlon(2) = plon(triangles(np,2))
    qlat(3) = plat(triangles(np,0))
    qlon(3) = plon(triangles(np,0))
    gsn_polyline(wks,map,qlon,qlat,gsres)
  end do

;
;  Mark the original data points with yellow circles.
;
  gsres@gsFillColor = 3
  do i=0,fsize-1
    nggcog(plat(i),plon(i),1.3,arclat,arclon)
    gsn_polygon(wks,map,arclon,arclat,gsres)
  end do

;
;  Mark the circumcircle centers with green dots.
;
  gsres@gsFillColor = 5
  do i=4,9
    nggcog(rlat(i),rlon(i),0.65,arclat,arclon)
    gsn_polygon(wks,map,arclon,arclat,gsres)
  end do

;
;  Legend
;
  txres = True
  txres@txFontHeightF = 0.025
  txres@txFontColor = 1
  txres@txJust = "CenterLeft"
  draw_box(wks, 0.02, 0.945, 0.12, 0.955, 2, gsres)
  gsn_text_ndc(wks, "~F22~Delaunay triangles", 0.14, 0.95, txres)
  draw_box(wks, 0.02, 0.895, 0.12, 0.905, 3, gsres)
  gsn_text_ndc(wks, "~F22~Voronoi polygons", 0.14, 0.90, txres)
  draw_box(wks, 0.02, 0.845, 0.12, 0.855, 4, gsres)
  gsn_text_ndc(wks, "~F22~Circumcircles", 0.14, 0.85, txres)

  frame(wks)

end


