
;***********************************************************************;
; file : gsun_scripts.ncl                                               ;
;                                                                       ;
; This file defines the functions and procedures used in the examples   ;
; in the "Getting started using NCL" documention.                       ;
;                                                                       ;
;***********************************************************************;

undef("ncl_open_ps")
undef("ncl_open_x11")
undef("ncl_open_ncgm")
undef("ncl_open_wks")
undef("ncl_define_colormap")
undef("ncl_contour")
undef("ncl_contour_map")
undef("ncl_map")
undef("ncl_polygon")
undef("ncl_polyline")
undef("ncl_polymarker")
undef("ncl_streamline")
undef("ncl_streamline_map")
undef("ncl_text")
undef("ncl_vector")
undef("ncl_vector_map")
undef("ncl_vector_scalar")
undef("ncl_vector_scalar_map")
undef("ncl_xy")

;***********************************************************************;
; Function : ncl_open_ncgm                                              ;
;               name : name of output cgm file                          ;
;                                                                       ;
; This function opens an NCGM output file called "<name>.ncgm" and      ;
; returns the workstation Id.                                           ;
;***********************************************************************;

function ncl_open_ncgm(name[1]:string)
local ncgm
begin

    if(name.eq."") then
        ncgm = create "ncgm" ncgmWorkstationClass defaultapp end create
    else
        ncgm = create name ncgmWorkstationClass defaultapp 
        "wkMetaName" : name + ".ncgm"
        end create
    end if

    return(ncgm)
end

;***********************************************************************;
; Function : ncl_open_x11                                               ;
;               name : name of X11 window                               ;
;                                                                       ;
; This function opens an X11 output window and returns the workstation  ;
; Id.                                                                   ;
;***********************************************************************;

function ncl_open_x11(name[1]:string)
local window
begin
    window = create name xWorkstationClass defaultapp
        "wkPause" : True
    end create

    return(window)
end

;***********************************************************************;
; Function : ncl_open_ps                                                ;
;               name : name of PostScript file                          ;
;                                                                       ;
; This function opens a PostScript file called "<name>.ps" and returns  ;
; the workstation Id.                                                   ;
;***********************************************************************;

function ncl_open_ps(type:string,name[1]:string)
local ps
begin
    if(name.eq."") then
        ps = create "gmeta" psWorkstationClass defaultapp
            "wkPSFileName" : "gmeta." + type
            "wkPSFormat" : type
        end create
    else
        ps = create name psWorkstationClass defaultapp
            "wkPSFileName" : name + "." + type
            "wkPSFormat" : type
        end create
    end if

    return(ps)
end

;***********************************************************************;
; Function : ncl_open_wks                                               ;
;               type : type of workstation to open                      ;
;               name : name of workstation                              ;
;                                                                       ;
; This function opens either an X11 window, an NCGM file, or a          ;
; PostScript file depending on "type", which can be "x11", "ncgm", or   ;
; "ps". If "type" is a PS file or an NCGM, then it will be named        ;
; <name>.ps or <name>.ncgm respectively. This function also looks for a ;
; resource file called "name.res". If it exists, then it loads the      ;
; resources defined in that file. This function returns the workstation ;
; Id.                                                                   ;
;***********************************************************************;

function ncl_open_wks(type[1]:string,name[1]:string)
local wks
begin
    appid = create name appClass defaultapp
        "appDefaultParent" : True
    end create

    if(type.eq."x11") then
        wks = ncl_open_x11(name)
    end if
    if(type.eq."ps".or.type.eq."eps".or.type.eq."epsi") then
        wks = ncl_open_ps(type,name)
    end if
    if(type.eq."ncgm") then
        wks = ncl_open_ncgm(name)
    end if
    wks@app = appid 
    wks@name = name
    return(wks)
end

;***********************************************************************;
; Procedure : ncl_define_colormap                                       ;
;                   wks: workstation object                             ;
;                  cmap: Colormap (n x 3 array)                         ;
;                                                                       ;
; This procedure defines a color map for workstation "wks" (the         ;
; variable returned from a previous call to "ncl_open_wks").            ;
;***********************************************************************;

procedure ncl_define_colormap(wks:graphic, cmap[*][*]: numeric)
begin
    setvalues wks
        "wkColorMap" : cmap
    end setvalues
end

;***********************************************************************;
; Function : ncl_contour                                                ;
;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
;             resources: optional resources                             ;
;                                                                       ;
; This function creates and draws a contour plot to the workstation     ;
; "wks" (the variable returned from a previous call to "ncl_open_wks"). ;
; "data" is the 2-dimensional data to be contoured, and "resources" is  ;
; an optional list of resources. The Id of the contour plot is returned.;
;***********************************************************************;

function ncl_contour(wks:graphic, data[*][*]:numeric, resources:logical )
local i, attnames, data_object, plot_object, res, sf_res_index, \
cn_res_index
begin
    calldraw = True
    callframe = True

; Create the data object.

    data_object = create wks@name + "_data" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues data_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wks@name + "_contour" contourPlotClass wks
        "cnScalarFieldData" : data_object
    end create

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

;***********************************************************************;
; Check to see if any data resources were set (these are resources that ;
; start with the two-letter index "sf") or if any contour resources were;
; set.  We assume that anything that doesn't start with an "sf" is a    ;
; contour or contour-related resource.                                  ;
;***********************************************************************;

        if(dimsizes(dimsizes(res)).eq.1)
            if(chartostring(res(0:1)).eq."sf")
                setvalues data_object
                    attnames : resources@$attnames$
                end setvalues
            else
                setvalues plot_object
                    attnames : resources@$attnames$
                end setvalues
            end if
        else
            cn_res_index = ind(chartostring(res(:,0:1)).ne."sf")
            sf_res_index = ind(chartostring(res(:,0:1)).eq."sf")
            if(.not.all(ismissing(sf_res_index)))
                do i = 0,dimsizes(sf_res_index)-1
                    setvalues data_object
                        attnames(sf_res_index(i)) : \
                                  resources@$attnames(sf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(cn_res_index)))
                do i = 0,dimsizes(cn_res_index)-1
                    setvalues plot_object
                        attnames(cn_res_index(i)) : \
                                  resources@$attnames(cn_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if
    if(calldraw.eq.True)
        draw(plot_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : ncl_contour_map                                            ;
;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a contour plot over a map plot to the ;
; workstation "wks" (the variable returned from a previous call to      ;
; "ncl_open_wks").  "data" is the 2-dimensional data to be contoured,   ;
; and "resources" is an optional list of resources. The Id of the map   ;
; plot is returned.                                                     ;
;***********************************************************************;

function ncl_contour_map(wks:graphic,data[*][*]:numeric,\
                         resources:logical)
local i, attnames, data_object, contour_object, res, sf_res_index, \
cn_res_index, mp_res_index, map_object
begin
    calldraw = True
    callframe = True

; Create the data object.

    data_object = create wks@name + "_data" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues data_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create plot object.

    contour_object = create wks@name + "_contour" contourPlotClass wks
        "cnScalarFieldData" : data_object
    end create

; Create map object.

    map_object = create wks@name + "_map" mapPlotClass wks
    end create

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))
        if(dimsizes(dimsizes(res)).eq.1)

;***********************************************************************;
; Check to see if any resources were set. For the data object, resources;
; begin with "sf". For the map object, resources begin with "mp". For   ;
; the contour object, any resource that doesn't start with "mp" or "sf" ;
; is assumed to be a contour or contour-related resource.               ;
;***********************************************************************;

            if(chartostring(res(0:1)).eq."sf")
                setvalues data_object
                    attnames : resources@$attnames$
                end setvalues
            else
                if(chartostring(res(0:1)).eq."mp")
                    setvalues map_object
                        attnames : resources@$attnames$
                    end setvalues
                else 
                    setvalues contour_object
                        attnames : resources@$attnames$
                    end setvalues
                end if
            end if
        else
            sf_res_index = ind(chartostring(res(:,0:1)).eq."sf")
            mp_res_index = ind(chartostring(res(:,0:1)).eq."mp")
            cn_res_index = ind(chartostring(res(:,0:1)).ne."mp".and.\
                               chartostring(res(:,0:1)).ne."sf")
            if(.not.all(ismissing(sf_res_index)))
                do i = 0,dimsizes(sf_res_index)-1
                    setvalues data_object
                        attnames(sf_res_index(i)) : \
                                  resources@$attnames(sf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(mp_res_index)))
                do i = 0,dimsizes(mp_res_index)-1
                    setvalues map_object
                        attnames(mp_res_index(i)) : \
                                  resources@$attnames(mp_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(cn_res_index)))
                do i = 0,dimsizes(cn_res_index)-1
                    setvalues contour_object
                        attnames(cn_res_index(i)) : \
                                  resources@$attnames(cn_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if
    overlay(map_object,contour_object)
    if(calldraw.eq.True)
        draw(map_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@contour = contour_object
    return(map_object)
end

;***********************************************************************;
; Function : ncl_map                                                    ;
;                      wks: workstation object                          ;
;               projection: Map projection                              ;
;                  resources: optional resources                        ;
;                                                                       ;
; This function creates and draws a map plot to the workstation "wks"   ;
; (the variable returned from a previous call to "ncl_open_wks").       ;
; "projection" is one of the ten supported map projections, and         ;
; "resources" is an optional list of resources. The Id of the map plot  ;
; is returned.                                                          ;
;***********************************************************************;

function ncl_map(wks:graphic, projection:string, resources:logical )
local i, attnames, plot_object
begin
    calldraw = True
    callframe = True

; Create plot object.

    plot_object = create wks@name + "_map" mapPlotClass wks
        "mpProjection" : projection
    end create

; Check to see if any resources were set.

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if
        attnames = getvaratts(resources)
        do i = 0,dimsizes(attnames)-1
            setvalues plot_object
                attnames(i) : resources@$attnames(i)$
            end setvalues
        end do
    end if
    if(calldraw.eq.True)
        draw(plot_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object.

    return(plot_object)
end

;***********************************************************************;
; Procedure : ncl_polygon                                               ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a filled polygon on the workstation "wks" (the   ;
; variable returned from a previous call to "ncl_open_wks") in the same ;
; data space as the data in "plotid" (returned from a previous call to  ;
; one of the ncl_* plotting functions). "x" and "y" are the x and y     ;
; locations of each point in the polygon, and should be in the same data;
; space as the data from "plotid". "resources" is an optional list of   ;
; resources.                                                            ;
;***********************************************************************;

procedure ncl_polygon(wks:graphic,plotid:graphic,x[*]:numeric,\
                      y[*]:numeric,resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

; Check to see if any data resources were set (these are resources that
; start with the two-letter index "gs").

        if(dimsizes(dimsizes(res)).eq.1)
            if(chartostring(res(0:1)).eq."gs")
                setvalues gsid
                    attnames : resources@$attnames$
                end setvalues
            end if
        else
            gs_res_index = ind(chartostring(res(:,0:1)).eq."gs")
            if(.not.all(ismissing(gs_res_index)))
                do i = 0,dimsizes(gs_res_index)-1
                    setvalues gsid
                        attnames(gs_res_index(i)) : \
                                  resources@$attnames(gs_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if

; Draw a polygon.

    NhlDataPolygon(plotid,gsid,x,y)
end

;***********************************************************************;
; Procedure : ncl_polyline                                              ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a polyline on the workstation "wks" (the variable;
; returned from a previous call to "ncl_open_wks") in the same data     ;
; space as the data in "plotid" (returned from a previous call to one of;
; the ncl_* plotting functions). "x" and "y" are the x and y locations  ;
; of each point in the line, and should be in the same data space as the;
; data from "plotid". "resources" is an optional list of resources.     ;
;***********************************************************************;

procedure ncl_polyline(wks:graphic,plotid:graphic,x[*]:numeric,\
                       y[*]:numeric,resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

; Check to see if any data resources were set (these are resources that
; start with the two-letter index "gs").

        if(dimsizes(dimsizes(res)).eq.1)
            if(chartostring(res(0:1)).eq."gs")
                setvalues gsid
                    attnames : resources@$attnames$
                end setvalues
            end if
        else
            gs_res_index = ind(chartostring(res(:,0:1)).eq."gs")
            if(.not.all(ismissing(gs_res_index)))
                do i = 0,dimsizes(gs_res_index)-1
                    setvalues gsid
                        attnames(gs_res_index(i)) : \
                                   resources@$attnames(gs_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if

; Draw a polyline.

    NhlDataPolyline(plotid,gsid,x,y)
end

;***********************************************************************;
; Procedure : ncl_polymarker                                            ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws polymarkers on the workstation "wks" (the        ;
; variable returned from a previous call to "ncl_open_wks") in the same ;
; data space as the data in "plotid" (returned from a previous call to  ;
; one of the ncl_* plotting functions). "x" and "y" are the x and y     ;
; locations of each marker, and should be in the same data space as the ;
; data from "plotid". "resources" is an optional list of resources.     ;
;***********************************************************************;

procedure ncl_polymarker(wks:graphic,plotid:graphic,x[*]:numeric,\
                         y[*]:numeric,resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

; Check to see if any data resources were set (these are resources that
; start with the two-letter index "gs").

        if(dimsizes(dimsizes(res)).eq.1)
            if(chartostring(res(0:1)).eq."gs")
                setvalues gsid
                    attnames : resources@$attnames$
                end setvalues
            end if
        else
            gs_res_index = ind(chartostring(res(:,0:1)).eq."gs")
            if(.not.all(ismissing(gs_res_index)))
                do i = 0,dimsizes(gs_res_index)-1
                    setvalues gsid
                        attnames(gs_res_index(i)) : \
                                   resources@$attnames(gs_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if

; Draw some polymarkers.

    NhlDataPolymarker(plotid,gsid,x,y)
end

;***********************************************************************;
; Function : ncl_streamline                                             ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot to the workstation  ;
; "wks" (the variable returned from a previous call to "ncl_open_wks"). ;
; "u" and "v" are the 2-dimensional arrays to be streamlined, and       ;
; "resources" is an optional list of resources. The Id of the streamline;
; plot is returned.                                                     ;
;***********************************************************************;

function ncl_streamline(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                        resources:logical)
local i,attnames,data_object,plot_object,res,vf_res_index,st_res_index
begin
    calldraw = True
    callframe = True

; Create the data object.

    data_object = create wks@name + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wks@name + "_stream" streamlinePlotClass wks
        "stVectorFieldData" : data_object
    end create

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

; Check to see if any data resources were set (these are resources that
; start with the two-letter index "vf").

        if(dimsizes(dimsizes(res)).eq.1)
            if(chartostring(res(0:1)).eq."vf")
                setvalues data_object
                    attnames : resources@$attnames$
                end setvalues
            else
                setvalues plot_object
                    attnames : resources@$attnames$
                end setvalues
            end if
        else
            vf_res_index = ind(chartostring(res(:,0:1)).eq."vf")
            st_res_index = ind(chartostring(res(:,0:1)).ne."vf")
            if(.not.all(ismissing(vf_res_index)))
                do i = 0,dimsizes(vf_res_index)-1
                    setvalues data_object
                        attnames(vf_res_index(i)) : \
                                   resources@$attnames(vf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(st_res_index)))
                do i = 0,dimsizes(st_res_index)-1
                    setvalues plot_object
                        attnames(st_res_index(i)) : \
                                   resources@$attnames(st_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if
    if(calldraw.eq.True)
        draw(plot_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : ncl_streamline_map                                         ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot over a map plot to  ;
; the workstation "wks" (the variable returned from a previous call to  ;
; "ncl_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; streamlined, and "resources" is an optional list of resources. The Id ;
; of the map plot is returned.                                          ;
;***********************************************************************;

function ncl_streamline_map(wks:graphic,u[*][*]:numeric,\
                            v[*][*]:numeric,resources:logical)
local i, attnames, data_object, contour_object, res, vf_res_index, \
st_res_index, mp_res_index, map_object
begin
    calldraw = True
    callframe = True

; Create the data object.

    data_object = create wks@name + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    stream_object = create wks@name + "_stream" streamlinePlotClass wks
        "stVectorFieldData" : data_object
    end create

; Create map object.

    map_object = create wks@name + "_map" mapPlotClass wks
    end create

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))
        if(dimsizes(dimsizes(res)).eq.1)

;***********************************************************************;
; Check to see if any resources were set. For the data object,          ;
; resources begin with "vf". For the map object, resources begin with   ;
; "mp". For the streamline object, any resource that doesn't start with ;
; "mp" or "vf" is assumed to be a streamline or streamline-related      ;
; resource.                                                             ;
;***********************************************************************;

            if(chartostring(res(0:1)).eq."vf")
                setvalues data_object
                    attnames : resources@$attnames$
                end setvalues
            else
                if(chartostring(res(0:1)).eq."mp")
                    setvalues map_object
                        attnames : resources@$attnames$
                    end setvalues
                else 
                    setvalues stream_object
                        attnames : resources@$attnames$
                    end setvalues
                end if
            end if
        else
            vf_res_index = ind(chartostring(res(:,0:1)).eq."vf")
            mp_res_index = ind(chartostring(res(:,0:1)).eq."mp")
            st_res_index = ind(chartostring(res(:,0:1)).ne."mp".and.\
                               chartostring(res(:,0:1)).ne."vf")
            if(.not.all(ismissing(vf_res_index)))
                do i = 0,dimsizes(vf_res_index)-1
                    setvalues data_object
                        attnames(vf_res_index(i)) : \
                                 resources@$attnames(vf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(mp_res_index)))
                do i = 0,dimsizes(mp_res_index)-1
                    setvalues map_object
                        attnames(mp_res_index(i)) : \
                                  resources@$attnames(mp_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(st_res_index)))
                do i = 0,dimsizes(st_res_index)-1
                    setvalues contour_object
                        attnames(st_res_index(i)) : \
                                   resources@$attnames(st_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if

    overlay(map_object,stream_object)
    if(calldraw.eq.True)
        draw(map_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@streamline = stream_object
    return(map_object)
end

;***********************************************************************;
; Procedure : ncl_text                                                  ;
;                   wks: workstation object                             ;
;                  text: array of text strings                          ;
;                     x: 1-dimensional array of x NDC positions for text;
;                     y: 1-dimensional array of y NDC positions for text;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws text strings on the workstation "wks" (the       ;
; variable returned from a previous call to "ncl_open_wks").  "x" and   ;
; "y" are the x and y locations of each text string, and should be      ;
; specified in view port coordinates (that is, values from 0.0 to 1.0). ;
; "resources" is an optional list of resources.                         ;
;***********************************************************************;

procedure ncl_text(wks:graphic, text:string, x:numeric, y:numeric, \
                   resources:logical )
local i, txid, attnames, plot_object, res, tx_res_index
begin

; Create text item objbect.

    txid = create wks@name + "_text" textItemClass wks
        "txString" : text
        "txPosXF"  : x
        "txPosYF"  : y
    end create

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

; Check to see if any text item resources were set (these are resources
; that start with the two-letter index "tx").

        if(dimsizes(dimsizes(res)).eq.1)
            if(chartostring(res(0:1)).eq."tx")
                setvalues txid
                    attnames : resources@$attnames$
                end setvalues
            end if
        else
            tx_res_index = ind(chartostring(res(:,0:1)).eq."tx")
            if(.not.all(ismissing(tx_res_index)))
                do i = 0,dimsizes(tx_res_index)-1
                    setvalues txid
                        attnames(tx_res_index(i)) : \
                                 resources@$attnames(tx_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if

; Draw text string.

    draw(txid)
end

;***********************************************************************;
; Function : ncl_vector                                                 ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot to the workstation "wks";
; (the variable returned from a previous call to "ncl_open_wks").  "u"  ;
; and "v" are the 2-dimensional arrays to be vectorized, and "resources";
; is an optional list of resources. The Id of the vector plot is        ;
; returned.                                                             ;
;***********************************************************************;

function ncl_vector(wks:graphic, u[*][*]:numeric, v[*][*]:numeric, \
                    resources:logical )
local i,attnames,data_object,plot_object,res,vf_res_index,vc_res_index
begin
    calldraw = True
    callframe = True

; Create the data object.

    data_object = create wks@name + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wks@name + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : data_object
    end create

; Check for existence of u@long_name and v@long_name and use them
; to label X and Y axes.

    if(isatt(u,"long_name")) then
        setvalues plot_object
            "tiXAxisString" : u@long_name
        end setvalues
    end if
    if(isatt(v,"long_name")) then
        setvalues plot_object
            "tiYAxisString" : v@long_name
        end setvalues
    end if
    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False    
        end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

; Check to see if any data resources were set (these are resources that
; start with the two-letter index "vf").

        if(dimsizes(dimsizes(res)).eq.1)
            if(chartostring(res(0:1)).eq."vf")
                setvalues data_object
                    attnames : resources@$attnames$
                end setvalues
            else
                setvalues plot_object
                    attnames : resources@$attnames$
                end setvalues
            end if
        else
            vf_res_index = ind(chartostring(res(:,0:1)).eq."vf")
            vc_res_index = ind(chartostring(res(:,0:1)).ne."vf")
            if(.not.all(ismissing(vf_res_index)))
                do i = 0,dimsizes(vf_res_index)-1
                    setvalues data_object
                        attnames(vf_res_index(i)) : \
                                resources@$attnames(vf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(vc_res_index)))
                do i = 0,dimsizes(vc_res_index)-1
                    setvalues plot_object
                        attnames(vc_res_index(i)) : \
                                resources@$attnames(vc_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if
    if(calldraw.eq.True)
        draw(plot_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : ncl_vector_map                                             ;
;                   wks: workstation object                             ;
;                     : 2-dimensional U data                            ;
;                     v: 2-dimensional V data                           ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot over a map plot to the  ;
; workstation "wks" (the variable returned from a previous call to      ;
; "ncl_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "resources" is an optional list of resources. The Id  ;
; of the map plot is returned.                                          ;
;***********************************************************************;

function ncl_vector_map(wks:graphic, u[*][*]:numeric, v[*][*]:numeric, \
                        resources:logical )
local i, attnames, data_object, contour_object, res, vf_res_index, \
vc_res_index, mp_res_index, map_object
begin
    calldraw = True
    callframe = True

; Create the data object.

    data_object = create wks@name + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    vector_object = create wks@name + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : data_object
    end create

; Create map object.

    map_object = create wks@name + "_map" mapPlotClass wks
    end create

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))
        if(dimsizes(dimsizes(res)).eq.1)

;***********************************************************************;
; Check to see if any resources were set. For the data object, resources;
; begin with "vf". For the map object, resources begin with "mp". For   ;
; the vector object, any resource that doesn't start with "mp" or "vf"  ;
; is assumed to be a vector or vector-related resource.                 ;
;***********************************************************************;

            if(chartostring(res(0:1)).eq."vf")
                setvalues data_object
                    attnames : resources@$attnames$
                end setvalues
            else
                if(chartostring(res(0:1)).eq."mp".or.\
                   chartostring(res(0:1)).eq."vp")
                    setvalues map_object
                        attnames : resources@$attnames$
                    end setvalues
                else 
                    setvalues vector_object
                        attnames : resources@$attnames$
                    end setvalues
                end if
            end if
        else
            vf_res_index = ind(chartostring(res(:,0:1)).eq."vf")
            mp_res_index = ind(chartostring(res(:,0:1)).eq."mp".or.\
                               chartostring(res(:,0:1)).eq."vp")
            vc_res_index = ind(chartostring(res(:,0:1)).ne."mp".and.\
                               chartostring(res(:,0:1)).ne."vf".and.\
                               chartostring(res(:,0:1)).ne."vp")
            if(.not.all(ismissing(vf_res_index)))
                do i = 0,dimsizes(vf_res_index)-1
                    setvalues data_object
                        attnames(vf_res_index(i)) : \
                                   resources@$attnames(vf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(mp_res_index)))
                do i = 0,dimsizes(mp_res_index)-1
                    setvalues map_object
                        attnames(mp_res_index(i)) : \
                                resources@$attnames(mp_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(vc_res_index)))
                do i = 0,dimsizes(vc_res_index)-1
                    setvalues vector_object
                        attnames(vc_res_index(i)) : \
                                 resources@$attnames(vc_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if
    overlay(map_object,vector_object)
    if(calldraw.eq.True)
        draw(map_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@vector = vector_object
    return(map_object)
end

;***********************************************************************;
; Function : ncl_vector_scalar                                          ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot to the workstation "wks";
; (the variable returned from a previous call to "ncl_open_wks").  "u"  ;
; and "v" are the 2-dimensional arrays to be vectorized, and "data" is  ;
; the scalar field that the vectors are colored by. "resources" is an   ;
; optional list of resources. The Id of the vector plot is returned.    ;
;***********************************************************************;

function ncl_vector_scalar(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                           data[*][*]:numeric, resources:logical )
local i, attnames, vfdata_object, sfdata_object, plot_object, res, \
vf_res_index, vc_res_index, sf_res_index
begin
    calldraw = True
    callframe = True

; Create the vector field data object.

    vfdata_object = create wks@name + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Create the scalar field data object.

    sfdata_object = create wks@name + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wks@name + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : vfdata_object
        "vcScalarFieldData" : sfdata_object
        "vcUseScalarArray"  : True
        "vcMonoLineArrowColor"  : "false"
    end create

; Check for existence of u@long_name and v@long_name and use them
; to label X and Y axes.

    if(isatt(u,"long_name")) then
        setvalues plot_object
            "tiXAxisString" : u@long_name
        end setvalues
    end if
    if(isatt(v,"long_name")) then
        setvalues plot_object
            "tiYAxisString" : v@long_name
        end setvalues
    end if
    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

; Check to see if any data resources were set (these are resources that
; start with the two-letter index "vf").

        if(dimsizes(dimsizes(res)).eq.1)
            if(chartostring(res(0:1)).eq."vf")
                setvalues vfdata_object
                    attnames : resources@$attnames$
                end setvalues
            else
                if(chartostring(res(0:1)).eq."sf")
                    setvalues sfdata_object
                        attnames : resources@$attnames$
                    end setvalues
                else
                    setvalues plot_object
                        attnames : resources@$attnames$
                    end setvalues
                end if
            end if
        else
            vf_res_index = ind(chartostring(res(:,0:1)).eq."vf")
            sf_res_index = ind(chartostring(res(:,0:1)).eq."sf")
            vc_res_index = ind(chartostring(res(:,0:1)).ne."vf".and.\
                               chartostring(res(:,0:1)).ne."sf")
            if(.not.all(ismissing(vf_res_index)))
                do i = 0,dimsizes(vf_res_index)-1
                    setvalues vfdata_object
                        attnames(vf_res_index(i)) : \
                                 resources@$attnames(vf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(sf_res_index)))
                do i = 0,dimsizes(sf_res_index)-1
                    setvalues sfdata_object
                        attnames(sf_res_index(i)) : \
                                  resources@$attnames(sf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(vc_res_index)))
                do i = 0,dimsizes(vc_res_index)-1
                    setvalues plot_object
                        attnames(vc_res_index(i)) : \
                                  resources@$attnames(vc_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if
    if(calldraw.eq.True)
        draw(plot_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object and data object (as attribute of plot object).

    plot_object@vfdata = vfdata_object
    plot_object@sfdata = sfdata_object
    return(plot_object)
end

;***********************************************************************;
; Function : ncl_vector_scalar_map                                      ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot over a map plot to the  ;
; workstation "wks" (the variable returned from a previous call to      ;
; "ncl_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "data" is the scalar field that the vectors are       ;
; colored by. "resources" is an optional list of resources. The Id of   ;
; the map plot is returned.                                             ;
;***********************************************************************;

function ncl_vector_scalar_map(wks:graphic,u[*][*]:numeric,\
                               v[*][*]:numeric,data[*][*]:numeric,\
                               resources:logical)
local i, attnames, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, mp_res_index, map_object
begin
    calldraw = True
    callframe = True
;
; Create the vector field object.
;
    vfdata_object = create wks@name + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

;
; Check for missing values.
;
    if(isatt(u,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create the scalar field object.

    sfdata_object = create wks@name + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create plot object.

    vector_object = create wks@name + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : vfdata_object
        "vcScalarFieldData" : sfdata_object
        "vcUseScalarArray"  : True
        "vcMonoLineArrowColor"  : "false"
    end create

; Create map object.

    map_object = create wks@name + "_map" mapPlotClass wks end create

    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))
        if(dimsizes(dimsizes(res)).eq.1)

;***********************************************************************;
; Check to see if any resources were set. For the vector field data     ;
; object, resources begin with "vf" and for the scalar field data       ;
; object, resources being with "sf". For the map object, resources begin;
; with "mp". For the vector object, any resource that doesn't start with;
; "mp" or "vf" is assumed to be a vector or vector-related resource.    ;
;***********************************************************************;

            if(chartostring(res(0:1)).eq."vf")
                setvalues vfdata_object
                    attnames : resources@$attnames$
                end setvalues
            else
                if(chartostring(res(0:1)).eq."sf")
                    setvalues ff_data_object
                        attnames : resources@$attnames$
                    end setvalues
                else
                    if(chartostring(res(0:1)).eq."mp".or.\
                       chartostring(res(0:1)).eq."vp")
                        setvalues map_object
                            attnames : resources@$attnames$
                        end setvalues
                    else 
                        setvalues vector_object
                        attnames : resources@$attnames$
                        end setvalues
                    end if
                end if
            end if
        else
            vf_res_index = ind(chartostring(res(:,0:1)).eq."vf")
            sf_res_index = ind(chartostring(res(:,0:1)).eq."sf")
            mp_res_index = ind(chartostring(res(:,0:1)).eq."mp".or.\
                               chartostring(res(:,0:1)).eq."vp")
            vc_res_index = ind(chartostring(res(:,0:1)).ne."mp".and.\
                               chartostring(res(:,0:1)).ne."vf".and.\
                               chartostring(res(:,0:1)).ne."sf".and.\
                               chartostring(res(:,0:1)).ne."vp")
            if(.not.all(ismissing(vf_res_index)))
                do i = 0,dimsizes(vf_res_index)-1
                    setvalues vfdata_object
                        attnames(vf_res_index(i)) : \
                              resources@$attnames(vf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(sf_res_index)))
                do i = 0,dimsizes(sf_res_index)-1
                    setvalues sfdata_object
                        attnames(sf_res_index(i)) : \
                               resources@$attnames(sf_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(mp_res_index)))
                do i = 0,dimsizes(mp_res_index)-1
                    setvalues map_object
                        attnames(mp_res_index(i)) : \
                              resources@$attnames(mp_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(vc_res_index)))
                do i = 0,dimsizes(vc_res_index)-1
                    setvalues vector_object
                        attnames(vc_res_index(i)) : \
                              resources@$attnames(vc_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if
    overlay(map_object,vector_object)
    if(calldraw.eq.True)
        draw(map_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata = vfdata_object
    map_object@sfdata = sfdata_object
    map_object@vector = vector_object
    return(map_object)
end

;***********************************************************************;
; Function : ncl_xy                                                     ;
;                   wks: workstation object                             ;
;                     x: n-dimensional array of X arrays                ;
;                     y: n-dimensional array of Y array                 ;
;             resources: optional resources                             ;
;                                                                       ;
; This function creates and draws an xy plot to the workstation "wks"   ;
; (the variable returned from a previous call to "ncl_open_wks").  "x"  ;
; and "y" are either 1 or 2-dimensional arrays containing the X and Y   ;
; data points and "resources" is an optional list of resources. The Id  ;
; of the xy plot is returned.                                           ;
;***********************************************************************;

function ncl_xy(wks:graphic, x:numeric, y:numeric, resources:logical )
local i, attnames, data_object, plot_object, res, ca_res_index, \
xy_res_index, xydp_res_index, dspec
begin
    calldraw = True
    callframe = True

; Determine if we have multiple lines or just one line.

    nxdims = dimsizes(dimsizes(x))
    xdims = dimsizes(x)
    data_object = create wks@name + "_data" coordArraysClass noparent
        "caXArray" : x
        "caYArray" : y
    end create

; Check for missing values.

    if(isatt(x,"_FillValue")) then
        setvalues data_object
            "caXMissingV" :x@_FillValue
        end setvalues
    end if
    if(isatt(y,"_FillValue")) then
        setvalues data_object
            "caYMissingV" :y@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wks@name + "_xy" xyPlotClass wks
        "xyCoordData" : data_object
    end create

; Check for existence of x@long_name and y@long_name and use them
; to label X and Y axes.

    if(isatt(x,"long_name")) then
        setvalues plot_object
            "tiXAxisString" : x@long_name
        end setvalues
    end if
    if(isatt(y,"long_name")) then
        setvalues plot_object
            "tiYAxisString" : y@long_name
        end setvalues
    end if

; By default, only solid lines get drawn if there are multiple lines, so
; set some dash patterns to use instead.

    getvalues plot_object
        "xyCoordDataSpec" : dspec
    end getvalues
    setvalues dspec
        "xyDashPatterns" : (/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/)
    end setvalues
    if((resources).and..not.any(ismissing(getvaratts(resources))))

; Check if frame and/or draw are not supposed to be called.

        if(any(getvaratts(resources).eq."Draw"))
            if(resources@Draw.eq.False)
                calldraw = False
            end if
            delete(resources@Draw)
        end if
        if(any(getvaratts(resources).eq."Frame"))
            if(resources@Frame.eq.False)
                callframe = False
            end if
            delete(resources@Frame)
        end if

; Get list of resources.

        attnames = getvaratts(resources)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

; Check to see if any data resources were set (these are resources that
; start with the two-letter index "ca").

        if(dimsizes(dimsizes(res)).eq.1)
            if(chartostring(res(0:1)).eq."ca")
                setvalues data_object
                    attnames : resources@$attnames$
                end setvalues
            end if
        else
            ca_res_index = ind(chartostring(res(:,0:1)).eq."ca")
            if(.not.all(ismissing(ca_res_index)))
                do i = 0,dimsizes(ca_res_index)-1
                    setvalues data_object
                        attnames(ca_res_index(i)) : \
                              resources@$attnames(ca_res_index(i))$
                    end setvalues
                end do
            end if
        end if

;***********************************************************************;
; Check to see if any xy plot resources were set.  There are two kinds  ;
; of xy plot resources, the regular kind, and the data spec kind.  If   ;
; the resource starts with an "xy", it could be either kind, so we need ;
; to have some tests to see which object it belongs to.  Any "xy"       ;
; resources that start with "xyCo", "xyX", or "xyY" are regular         ;
; resources (meaning, it belongs to the XyPlot object). The remaining   ;
; "xy" resources belong to the data spec object. Any resources that do  ;
; not start with "xy" or "ca" are assumed to also go with the XyPlot    ;
; object.                                                               ;
;***********************************************************************;

        if(dimsizes(dimsizes(res)).eq.1)
            if((chartostring(res(0:1)).ne."ca".and.\
                chartostring(res(0:1)).ne."xy").or.\
               (chartostring(res(0:1)).eq."xy".and.\
               (chartostring(res(0:3)).eq."xyCo".or.\
                chartostring(res(0:2)).eq."xyX".or.\
                chartostring(res(0:2)).eq."xyY")))
                setvalues plot_object
                    attnames : resources@$attnames$
                end setvalues
            end if
            if(chartostring(res(0:1)).eq."xy".and.\
              (chartostring(res(0:3)).ne."xyCo".and.\
               chartostring(res(0:2)).ne."xyX".and.\
               chartostring(res(0:2)).ne."xyY"))
                setvalues dspec
                    attnames : resources@$attnames$
                end setvalues
            end if
        else
            xy_res_index = ind((chartostring(res(:,0:1)).ne."ca".and.\
                                chartostring(res(:,0:1)).ne."xy").or.\
                               (chartostring(res(:,0:1)).eq."xy".and.\
                               (chartostring(res(:,0:3)).eq."xyCo".or.\
                                chartostring(res(:,0:2)).eq."xyX".or.\
                                chartostring(res(:,0:2)).eq."xyY")))
            xydp_res_index = ind(chartostring(res(:,0:1)).eq."xy".and.\
                                (chartostring(res(:,0:3)).ne."xyCo".and.\
                                 chartostring(res(:,0:2)).ne."xyX".and.\
                                 chartostring(res(:,0:2)).ne."xyY"))
            if(.not.all(ismissing(xy_res_index)))
                do i = 0,dimsizes(xy_res_index)-1
                    setvalues plot_object
                        attnames(xy_res_index(i)) : \
                                  resources@$attnames(xy_res_index(i))$
                    end setvalues
                end do
            end if
            if(.not.all(ismissing(xydp_res_index)))
                getvalues plot_object
                    "xyCoordDataSpec" : dspec
                end getvalues
                do i = 0,dimsizes(xydp_res_index)-1
                    setvalues dspec
                        attnames(xydp_res_index(i)) : \
                                resources@$attnames(xydp_res_index(i))$
                    end setvalues
                end do
            end if
        end if
    end if
    if(calldraw.eq.True)
        draw(plot_object)
    end if
    if(callframe.eq.True)
        frame(wks)
    end if

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end
