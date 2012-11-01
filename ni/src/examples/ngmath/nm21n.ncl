;
;      $Id: nm21n.ncl,v 1.8 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  2000                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm21n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Thu Jun  3 15:16:01 MDT 1999
;
;   Description:  This program illustrates the use of the interpolation
;                 capabilities of the cssgrid package.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

procedure nrand(first:integer, nextn:integer, result:float)
;
;  Portable random number generator.  This is here in place
;  of the built-in random number generator "rand"
;  so that the results for this example will be the same
;  as for the equivalent Fortran and C examples.
;
;  Arguments
;    first  - is 0 if this is the first call to the procedure, 1 otherwise
;    nextn  - a temporary storage variable that should be the same for
;             all calls.
;    result - the desired random number 
;  
local mplier, modlus, mobymp, momdmp, hvlue, lvlue, testv
begin

  mplier =      16807
  modlus = 2147483647
  mobymp =     127773
  momdmp =       2836

  if (first .eq. 0) then
    nextn = 9
  end if

  hvlue = nextn / mobymp
  lvlue = nextn%mobymp;
  testv = mplier*lvlue - momdmp*hvlue;
  if (testv .gt. 0) then
    nextn = testv;
  else 
    nextn = testv + modlus;
  end if

  result = (1. * nextn) / (1. * modlus)
end

procedure genrs(rlat[*]:float, rlon[*]:float)
;
;  Generate random positions on a sphere in latitudes and longitudes
;  (latitude values between -90. and 90. and longitude values
;  between -180. and 180).
;
;  First select a random longitude, and then select a random value
;  on the Z axis -R to R (-1 to 1 in the case of the unit sphere).
;  From the random Z value, calculate the latitude.
;
local size, tmpa, rslt, rz

begin
  size = dimsizes(rlat)
  tmpa = new(1,integer)
  rslt = new(1,float)
  nrand(0,tmpa,rslt)
  rlon(0) = -180.+360.*rslt
  nrand(1,tmpa,rslt)
  rz = 2.*rslt-1.
  rlat(0) = 57.29578*asin(rz)
  do i=1,size-1
    nrand(1,tmpa,rslt)
    rlon(i) = -180.+360.*rslt
    nrand(1,tmpa,rslt)
    rz = 2.*rslt-1.
    rlat(i) = 57.29578*asin(rz)
  end do
end

function max2a(a1[*]:numeric, a2[*]:numeric)
begin
  if (a1 .lt. a2) then
    return(a2)
  else
    return(a1)
  end if
end
function min2a(a1[*]:numeric, a2[*]:numeric)
begin
  if (a1 .lt. a2) then
    return(a1)
  else
    return(a2)
  end if
end

procedure geni(mlow[1]:integer, mhgh[1]:integer, \
               dlow[1]:float, dhgh[1]:float, space[645]:float)
;
;  The procedure geni and the function genpnt are used to generate 
;  test ;  data on the globe.  The data generated will have 
;  approximately "mlow" lows and "mhgh" highs within each of 
;  the twenty areas defined by an inscribed icosahedron, a minimum 
;  value of approximately "dlow" and a maximum value of approximately
;  "dhgh".  "space" is a storage area for communication between geni
;  and genpnt.  geni is called to initialize the process and
;  the function genpnt returns a value of the function at
;  the point (rlat,rlon).
;
;  The function used is a sum of exponentials.
;
;  Versions of these codes were originally written in Fortran
;  David Kennison at NCAR.
;

local D2R, R2D, r0, r1, r2, xcvi, ycvi, zcvi, ivof, nlow, nhgh,   \
      ncnt, indx, wgt1, wgt2, wgt3, wsum, xtmp, ytmp, ztmp, tmmp, \
      ilon, ilat, rlon, crln, srln, rlat, crlt, srlt, rval, dist, \
      angl, xcoc, ycoc, zcoc, rmul, rhgh, rlow
begin

  D2R =  0.017453293
  R2D = 57.295780000

  r0 = (/                                                          \
     .968,.067,.478,.910,.352,.933,.654,.021,.512,.202,.940,.204,  \
     .379,.793,.288,.267,.357,.128,.703,.737,.803,.915,.511,.762,  \
     .456,.471,.300,.613,.073,.498,.220,.041,.565,.698,.951,.917,  \
     .630,.605,.938,.143,.807,.878,.347,.186,.671,.635,.453,.028,  \
     .763,.157,.765,.566,.072,.276,.328,.528,.747,.627,.141,.821,  \
     .126,.360,.862,.690,.058,.813,.607,.689,.419,.545,.831,.226,  \
     .422,.178,.412,.093,.813,.866,.121,.576,.023,.886,.142,.095,  \
     .162,.470,.623,.910,.097,.764,.730,.223,.124,.593,.913,.183,  \
     .406,.520,.871,.825,.065,.703,.051,.488,.881,.463,.581,.694,  \
     .329,.702,.270,.352,.587,.412,.446,.750,.882,.069,.659,.979,  \
     .833,.390,.202,.957,.982,.115,.140,.389,.635,.011,.213,.701,  \
     .714,.264,.188,.594,.727,.769,.288,.056,.471,.558,.408,.058,  \
     .970,.854,.808,.851,.923,.467,.830,.756,.857,.032,.713,.839,  \
     .147,.852,.228,.783,.863,.441,.483,.577,.705,.671,.171,.432,  \
     .441,.459,.489,.911,.017,.897,.969,.987,.751,.777,.838,.674,  \
     .244,.669,.430,.101,.701,.143,.940,.848,.995,.168,.631,.858,  \
     .608,.114,.435,.313,.785,.606,.746,.226,.065,.234,.137,.082,  \
     .131,.106,.069,.882,.883,.907,.556,.127,.576,.986,.228,.276,  \
     .128,.168,.124,.123                                           \
    /)

  r1 = (/                                                          \
     .023,.003,.880,.088,.237,.017,.170,.368,.123,.239,.250,.006,  \
     .146,.806,.134,.722,.791,.361,.998,.920,.529,.122,.043,.864,  \
     .877,.025,.808,.746,.442,.065,.400,.464,.068,.280,.552,.305,  \
     .297,.722,.673,.420,.961,.923,.426,.107,.729,.560,.829,.520,  \
     .921,.827,.440,.451,.950,.483,.315,.827,.508,.123,.573,.949,  \
     .188,.973,.414,.256,.253,.966,.561,.550,.689,.234,.970,.650,  \
     .157,.396,.757,.885,.956,.587,.405,.877,.414,.845,.328,.363,  \
     .328,.643,.190,.836,.766,.763,.786,.954,.737,.199,.211,.990,  \
     .165,.772,.540,.854,.006,.510,.504,.163,.906,.261,.048,.862,  \
     .848,.454,.740,.262,.299,.068,.625,.627,.711,.815,.464,.477,  \
     .579,.249,.431,.315,.449,.642,.305,.614,.414,.845,.468,.420,  \
     .355,.972,.582,.261,.234,.631,.123,.082,.084,.863,.343,.383,  \
     .930,.968,.011,.641,.784,.474,.118,.362,.723,.549,.678,.172,  \
     .191,.983,.786,.605,.828,.254,.024,.183,.226,.607,.444,.460,  \
     .237,.567,.541,.322,.430,.885,.705,.361,.853,.715,.002,.637,  \
     .190,.119,.999,.913,.668,.677,.085,.859,.660,.871,.464,.488,  \
     .124,.489,.671,.350,.095,.115,.810,.333,.683,.351,.654,.113,  \
     .236,.359,.473,.089,.075,.475,.726,.264,.594,.725,.177,.263,  \
     .402,.262,.122,.062                                           \
    /)

  r2 = (/                                                          \
     .337,.417,.503,.020,.769,.158,.133,.005,.517,.606,.094,.591,  \
     .081,.820,.855,.675,.545,.033,.938,.947,.294,.060,.009,.427,  \
     .646,.559,.684,.721,.781,.291,.892,.118,.708,.395,.138,.476,  \
     .552,.270,.481,.069,.877,.575,.660,.957,.395,.516,.633,.939,  \
     .548,.570,.886,.843,.630,.895,.270,.276,.455,.953,.998,.236,  \
     .244,.889,.354,.952,.284,.492,.428,.837,.762,.909,.906,.639,  \
     .484,.566,.596,.879,.082,.229,.818,.631,.799,.704,.473,.430,  \
     .600,.743,.706,.055,.696,.704,.291,.940,.593,.645,.892,.877,  \
     .137,.321,.714,.899,.230,.620,.538,.714,.186,.134,.593,.268,  \
     .364,.411,.899,.163,.116,.372,.593,.716,.115,.298,.770,.812,  \
     .002,.061,.752,.595,.706,.645,.472,.843,.965,.186,.742,.195,  \
     .806,.280,.910,.992,.414,.503,.260,.778,.915,.159,.941,.030,  \
     .531,.533,.746,.647,.832,.516,.458,.834,.577,.211,.429,.283,  \
     .855,.901,.126,.821,.087,.868,.016,.893,.148,.926,.885,.562,  \
     .429,.145,.340,.343,.304,.281,.374,.835,.814,.120,.482,.646,  \
     .636,.940,.479,.213,.151,.908,.497,.006,.809,.623,.827,.895,  \
     .490,.843,.788,.638,.769,.673,.200,.198,.817,.540,.541,.121,  \
     .821,.915,.956,.635,.035,.438,.280,.671,.377,.760,.884,.528,  \
     .668,.381,.534,.477                                           \
    /)

  xcvi = (/                                                        \
     .9510565162952 , -.9510565162951 ,  .4253254041760 ,          \
    -.4253254041760 ,  .4253254041760 , -.4253254041760 ,          \
     .4253254041760 , -.4253254041760 ,  .4253254041760 ,          \
    -.4253254041760 ,  .4253254041760 , -.4253254041760            \
    /)

  ycvi = (/                                                        \
     .0000000000000 ,  .0000000000000 ,  .8506508083520 ,          \
    -.8506508083520 ,  .2628655560596 , -.2628655560596 ,          \
    -.6881909602356 ,  .6881909602356 , -.6881909602356 ,          \
     .6881909602356 ,  .2628655560595 , -.2628655560596            \
    /)

  zcvi = (/                                                        \
     .0000000000000 ,  .0000000000000 ,  .0000000000000 ,          \
     .0000000000000 ,  .8090169943749 , -.8090169943749 ,          \
     .5000000000000 , -.5000000000000 , -.5000000000000 ,          \
     .5000000000000 , -.8090169943749 ,  .8090169943749            \
    /)

  ivof = (/                                                        \
            (/0, 2, 4/)  , (/0, 4, 6/)  , (/0, 6, 8/) ,            \
            (/0, 8,10/)  , (/0, 2,10/)  , (/2, 7,10/) ,            \
            (/2, 7, 9/)  , (/2, 4, 9/)  , (/4, 9,11/) ,            \
            (/4, 6,11/)  , (/6, 3,11/)  , (/3, 6, 8/) ,            \
            (/3, 5, 8/)  , (/5, 8,10/)  , (/5, 7,10/) ,            \
            (/5, 1, 7/)  , (/1, 3, 5/)  , (/1, 3,11/) ,            \
            (/1, 9,11/)  , (/1, 7, 9/)                             \
         /)

  nlow = max2a(0,min2a(4,mlow))
  nhgh = max2a(0,min2a(4,mhgh))
  ncnt = 20*(nlow+nhgh)
  space(644) = 1.*ncnt
  do k=0,ncnt-1
    indx = k%20
    wgt1 = r0(k)
    wgt2 = r1(k)
    wgt3 = r2(k)
    wsum = wgt1+wgt2+wgt3
    wgt1 = wgt1/wsum 
    wgt2 = wgt2/wsum 
    wgt3 = wgt3/wsum 
    xtmp = wgt1*xcvi(ivof(indx,0)) +   \
           wgt2*xcvi(ivof(indx,1)) +   \
           wgt3*xcvi(ivof(indx,2))
    ytmp = wgt1*ycvi(ivof(indx,0)) +   \
           wgt2*ycvi(ivof(indx,1)) +   \
           wgt3*ycvi(ivof(indx,2))
    ztmp = wgt1*zcvi(ivof(indx,0)) +   \
           wgt2*zcvi(ivof(indx,1)) +   \
           wgt3*zcvi(ivof(indx,2))
    temp = sqrt(xtmp*xtmp +ytmp*ytmp + ztmp*ztmp)
    space(160+k) = xtmp/temp
    space(320+k) = ytmp/temp
    space(480+k) = ztmp/temp
  end do
  bp = 20*nlow-1
  space(0:bp) = -1.
  space(bp+1:ncnt-1) =  1.

  space(640) = dlow;
  space(641) = dhgh;
  space(642) =  1.e+36;
  space(643) = -1.e+36;
  
  xcoc = space(160:160+ncnt-1)
  ycoc = space(320:320+ncnt-1)
  zcoc = space(480:480+ncnt-1)
  rmul = space(0:ncnt-1)
  rhgh = space(641)
  rlow = space(640)
  do i=0,71
    ilon = -180.+5.*i
    rlon = D2R * 1.*ilon
    crln = cos(rlon)
    srln = sin(rlon)
    do j=0,34
      rlat = D2R*(-85.+5.*j)
      crlt = cos(rlat)
      srlt = sin(rlat)
      rval = 0.5*(rhgh+rlow)
      dist = sqrt((crln*crlt-xcoc)^2 + (srln*crlt-ycoc)^2 + (srlt-zcoc)^2)
      angl = 2.*R2D*asin(0.5*dist)
      dist = angl/18.
      tval = 0.5*(rhgh-rlow)*rmul*2.7182818^(-dist*dist)*(2.-sin(6.283*dist)/2.)
      rval = rval + sum(tval)
      space(642) = min2a(space(642),rval)
      space(643) = max2a(space(643),rval)
    end do
  end do
end

function genpnt(rlat[1]:float, rlon[1]:float, space[645]:float)
;
;  This function returns a functional value at the specified
;  lat/lon coordinate.  The function is determined by the
;  initial call to geni (see above).
;
local crlt, crln, srln, srlt, rval, dist, angl, ncnt, R2D,  \
      xcoc, ycoc, zcoc, rmul, rhgh, rlow
begin
  R2D = 57.295780000

  crlt = cos(rlat)
  crln = cos(rlon)
  srlt = sin(rlat)
  srln = sin(rlon)
  rhgh = space(641)
  rlow = space(640)
  ncnt = floattointeger(space(644))
  xcoc = space(160:160+ncnt-1)
  ycoc = space(320:320+ncnt-1)
  zcoc = space(480:480+ncnt-1)
  rmul = space(0:ncnt-1)
  rval = 0.5*(rhgh+rlow)
  dist = sqrt((crln*crlt-xcoc)^2 + (srln*crlt-ycoc)^2 + (srlt-zcoc)^2)
  angl = 2.*R2D*asin(0.5*dist)
  dist = angl/18.
  tval = 0.5*(rhgh-rlow)*rmul*2.7182818^(-dist*dist)*(2.-sin(6.283*dist)/2.)
  rval = rval + sum(tval)
  return(space(640)+(space(641)-space(640))*  \
         (rval-space(642))/(space(643)-space(642)))
end

;
;  Main program
;
begin
 
  D2R =  0.017453293
  R2D = 57.295780000

;
;  Define random points and functional values on the globe,
;  triangulate, interpolate to a uniform grid, then draw a 
;  contour plot on a map.
;

;
;  Number of input data values.
;
  N = 500

;
;  Number of points to use for drawing arcs on the globe.
;
  NARC = 50

;
;  Array sizes for the interpolation grid.
;
  NI =  73
  NJ = 145

;
;  Generate a default set of nodes as latitudinal and longitudinal
;  coordinates (latitudes in the range -90. to 90. and longitudes
;  in the range -180. to 180).
;
  rlat = new(N,float)
  rlon = new(N,float)
  genrs(rlat, rlon)

;
;  Generate functional values at the input nodes.
;
  fval = new(N,float)
  tmp_space = new(645,float)
  geni(5, 10, -200., 500., tmp_space)
  do i=0,N-1
    fval(i) = genpnt(D2R*rlat(i), D2R*rlon(i), tmp_space)
  end do

;
;  Create the triangulation.
;
  triangles = csstri(rlat,rlon)
  tri_sizes = dimsizes(triangles)
  num_triangles = tri_sizes(0)

;
;  Get the circumcenters for the Delaunay triangles and store
;  them in arrays plat and plon.  "nca" is the actual number 
;  of circles found.
;
  plat = new(2*N,float)
  plon = new(2*N,float)
  rc = new(2*N,float)
  nca = new(1,integer)
  numv = new(1,integer)
  nv = new(N,integer)
  csvoro(rlat,rlon,0,1,plat,plon,rc,nca,numv,nv)

;  Draw a plot of the triangulation and the Voronoi polygons.
;

;
;  Define a color map and open a workstation.
;
  cmap = (/                      \
            (/ 1., 1., 1. /),    \
            (/ 0., 0., 0. /),    \
            (/ 1., 0., 0. /),    \
            (/ 0., 0., 1. /),    \
            (/ 1., 0., 0. /),    \
            (/ 0., 1., 0. /),    \
            (/ 0., .8, 0. /),    \
            (/ .65, .65, .65 /)  \
         /)

  wks_type = "ncgm"
  wks = gsn_open_wks(wks_type,"nm21n")
  gsn_define_colormap(wks,cmap)

;
;  Define some resources and draw a globe as a background for 
;  the plot.
;
  map_resources = True
  map_resources@gsnFrame = False
  map_resources@mpOutlineBoundarySets = "National"
  map_resources@mpNationalLineColor = 1
  map_resources@mpGeophysicalLineColor = 7

  map_resources@mpLimbLineColor = 7
  map_resources@mpGridLineColor = 0
  map_resources@mpGridAndLimbDrawOrder = "PreDraw"

  map_resources@mpCenterLatF =   40.
  map_resources@mpCenterLonF = -105.
  map_resources@vpXF = 0.06
  map_resources@vpYF = 0.90
  map_resources@vpWidthF = 0.88
  map_resources@vpHeightF = 0.88
  map_resources@mpSatelliteDistF  = 4.0

  map_resources@mpGreatCircleLinesOn = True

  map = gsn_map(wks,"Satellite",map_resources)

;
;  Draw the Voronoi polygons.
;
  gsres = True
  gsres@gsLineColor = 3
  rlatn = new(2,float)
  rlonn = new(2,float)
  do i=0,N-1
    csvoro(rlat,rlon,i,0,plat,plon,rc,nca,numv,nv)
    do j=1,numv-1
      rlatn(0) = plat(nv(j-1))
      rlonn(0) = plon(nv(j-1))
      rlatn(1) = plat(nv(j))
      rlonn(1) = plon(nv(j))
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
    qlat(0) = rlat(triangles(np,0))
    qlon(0) = rlon(triangles(np,0))
    qlat(1) = rlat(triangles(np,1))
    qlon(1) = rlon(triangles(np,1))
    qlat(2) = rlat(triangles(np,2))
    qlon(2) = rlon(triangles(np,2))
    qlat(3) = rlat(triangles(np,0))
    qlon(3) = rlon(triangles(np,0))
    gsn_polyline(wks,map,qlon,qlat,gsres)
  end do

;
;  Mark the original data points with black circles.
;
  gsres@gsLineColor = 1
  arclat = new(NARC, float)
  arclon = new(NARC, float)
  do i=0,N-1
    do j=1,6
      nggcog(rlat(i),rlon(i),0.15*j,arclat,arclon)
      gsn_polyline(wks,map,arclon,arclat,gsres)
    end do
  end do

;
;  Title
;
  txres = True
  txres@txFontHeightF = 0.035
  txres@txFontColor = 1
  txres@txJust = "CenterCenter"
  gsn_text_ndc(wks, "~F26~Triangulation", 0.5, 0.95, txres)

  frame(wks)

;
;  Set up the latitudes and longitudes for the interpolated grid.
;
  platn = new(NI,float) 
  plonn = new(NJ,float) 
  do i=0,NI-1
    platn(i) = -90.+i*2.5
  end do
  do j=0,NJ-1
    plonn(j) = -180.+j*2.5
  end do

;
;  Interpolate to the regular grid.
;
  ff = cssgrid(rlat,rlon,fval,platn,plonn)

;
;  Draw a contour map of the interpolated values using the same 
;  map projection and resource settings as used for drawing the 
;  triangulation above.
;
  map_resources = True
  map_resources@mpOutlineBoundarySets = "National"
  map_resources@mpOutlineSpecifiers = "USStatesLand"
  map_resources@mpNationalLineColor = 7
  map_resources@mpUSStateLineColor = 7
  map_resources@mpGeophysicalLineColor = 7
  map_resources@cnLineColor = 3
 
  map_resources@cnLevelSelectionMode = "AutomaticLevels"
  map_resources@cnLevelSpacingF  = 40.
  map_resources@cnMaxLevelCount = 16
  map_resources@cnLineLabelPlacementMode = "Constant"
  map_resources@cnLineLabelFontHeightF = 0.01
  map_resources@cnLineLabelInterval = 5
  map_resources@cnLevelFlags = (/ 1,1,1,1,3,1,1,1,1,3,1,1,1,1,3,1,1,1,1,3 /)
  map_resources@cnLineLabelFontColor = 3
  map_resources@cnLineLabelFont = 13

  map_resources@cnInfoLabelOn = False
 
  map_resources@mpCenterLatF =   40.
  map_resources@mpCenterLonF = -105.
  map_resources@vpXF = 0.06
  map_resources@vpYF = 0.90
  map_resources@vpWidthF  = 0.88
  map_resources@vpHeightF = 0.88
 
  map_resources@mpProjection = "Satellite"
  map_resources@mpSatelliteDistF  = 4.
  map_resources@sfXCStartV = -180.
  map_resources@sfXCEndV   =  180.
  map_resources@sfYCStartV =  -90.
  map_resources@sfYCEndV   =   90.
 
  map_resources@cnSmoothingOn = True
  map_resources@cnSmoothingTensionF = 0.02
 
  map_resources@mpGridLineColor = 0
  map_resources@mpLimbLineColor = 3
  map_resources@mpGridAndLimbDrawOrder = "PreDraw"
 
  map = gsn_contour_map(wks,ff,map_resources)

;
;  Title
;
  txres = True
  txres@txFontHeightF = 0.035
  txres@txFontColor = 1
  txres@txJust = "CenterCenter"
  gsn_text_ndc(wks, "~F26~Contour Plot of Gridded Data", 0.5, 0.95, txres)

  frame(wks)

end
