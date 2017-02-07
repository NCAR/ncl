;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_color_country_user.ncl
; 
;  This example requires the Climate Data Operators (cdo), which
;  can be downloaded from: https://code.zmaw.de/projects/cdo/files
;
;  KMF 
;-----------------------------------------------------------------
begin
  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "data_country_user.txt"

  if (fileexists(diri+fili)) then
     data =  asciiread(diri+fili, -1,"string" );-- read all lines
  else
     print("")
     print("You don't have the necessary data for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     exit
  end if
  
  delim   = ";"                                ;-- set delimiter
  nfields =  str_fields_count(data(0),delim)   ;-- count number of columns
 
  states  =  str_get_field(data,1,delim)        ;-- get 1st column
  ivalues =  toint(str_get_field(data,2,delim)) ;-- get 2nd column
  nvalues = dimsizes(ivalues)

  levels  =  (/1,2,5,10,50,100,200,500,1000,2000/) ;-- value levels
  labels  =  (/"1","2",">5",">10",">50",">100",">200", \
               ">500",">1000",">2000"/)        ;-- labelbar labels

  rgb_colors = (/ (/0.997785, 0.999139, 0.846059/), \
                  (/0.910127, 0.964937, 0.695640/), \
                  (/0.769320, 0.909419, 0.706959/), \
                  (/0.521292, 0.812964, 0.731073/), \
                  (/0.304483, 0.732118, 0.761430/), \
                  (/0.141961, 0.597647, 0.756078/), \
                  (/0.122107, 0.483137, 0.712711/), \
                  (/0.131949, 0.382745, 0.665467/), \
                  (/0.138408, 0.297578, 0.624990/), \
                  (/0.031373, 0.113725, 0.345098/)/)
  nlevels =  dimsizes(levels)                    ;-- number of levels
  colors  =  new((/nvalues+1,3/),typeof(rgb_colors))

;-- compute the data color array
  do i = 0,dimsizes(ivalues)-1
     if(ivalues(i).eq.levels(0)) then
        colors(i,:) = (/1.,1.,1./)         ; white
     end if
     if(ivalues(i).gt.levels(nlevels-1)) then
        colors(i,:) = rgb_colors(nlevels-1,:)
     end if
     do j = 0,nlevels-2
        if(ivalues(i).gt.levels(j).and.ivalues(i).le.levels(j+1)) then
           colors(i,:) = rgb_colors(j,:)
        end if
     end do
     print("State: "+sprinti("%2.2i",i)+"   Count:  "+ \
             sprinti("%4.2i",ivalues(i))+"  "+states(i))
  end do

;-- open a workstation
;-- set workstation resources
  wks_type                 = "png"        ;-- plot output type
  wks_type@wkWidth         =  1600        ;-- wk width
  wks_type@wkHeight        =  1600        ;-- wk height
  wks = gsn_open_wks(wks_type,"NUG_color_country_user")

;-- set resources
  res                       =  True  
  res@gsnMaximize           =  True        ;-- maximize plot
  res@gsnFrame              =  False       ;-- don't advance the frame yet

  res@pmTickMarkDisplayMode = "Always"     ;-- turn on map tickmarks

  res@mpDataSetName         = "Earth..4"   ;-- new database
  res@mpDataBaseVersion     = "MediumRes"  ;-- Medium resolution database
  res@mpOutlineOn           =  True        ;-- turn on map outlines
  res@mpFillOn              =  True        ;-- turn on map fill
  res@mpOutlineBoundarySets = "National"   ;-- draw only national bounds
  res@mpLandFillColor       = "white"      ;-- set map land fill to white
  res@mpMinLatF             = -60          ;-- don't plot Antarctica
;-- set colors and states
  res@mpFillAreaSpecifiers  =  states      ;-- fill listed states
  res@mpSpecifiedFillColors =  colors       ;-- use generated color array

  res@tiMainString          = "User Count" ;-- title string
  res@tiMainFont            = "helvetica"  ;-- title string font
  res@tiMainFontHeightF     =  0.025       ;-- set title string font size

  map = gsn_csm_map(wks,res)    ;-- create the map

;-- add custom label bar to the plot
  lbres                     =  True
  lbres@lbPerimOn           =  False       ;-- no label bar box outline
  lbres@lbOrientation       = "Horizontal" ;-- labelbar orientation
  lbres@vpXF                =  0.01        ;-- labelbar x-position
  lbres@vpYF                =  0.26        ;-- labelbar y-position
  lbres@vpWidthF            =  0.98        ;-- labelbar width
  lbres@vpHeightF           =  0.08        ;-- labelbar height
  lbres@lbLabelFontHeightF  =  0.012       ;-- label font height
  lbres@lbMonoFillPattern   =  True        ;-- fill sold
  lbres@lbAutoManage        =  False       ;-- make settings by yourself
  lbres@lbLabelAlignment    = "BoxCenters" ;-- where to draw the labelbar             
                                               ;labels
  lbres@lbFillColors        =  rgb_colors  ;-- use colors

  gsn_labelbar_ndc(wks,nlevels,labels,0.13,0.28,lbres) ;-- draw labelbar
         
  frame(wks)                               ;-- advance the frame

end
