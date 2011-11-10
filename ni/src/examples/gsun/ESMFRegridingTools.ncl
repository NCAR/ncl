; The following functions and procedures are written by:
;        Mohammad Abouali, maboualiedu@gmail.com
;              May 22nd, July 30th, 2011
;
;        http://sites.google.com/site/maboualihome/
;
; This script was added to the NCL trunk on 13 Oct 2011.
;
;    Modifications:
;       09 Nov 2011: 
;            Decided to completely remove "remove outlier" code.
;            Cleaned up the sparse_matrix_mult code to correctly
;            handle the case of the source grid being 1D.
;
;       06 Nov 2011: 
;            Made code faster by adding option
;            ("RemoveOutliers", False by default) to not
;            remove outliers. It turns out this code was
;            added to fix bug in original SMM code in
;            handling missing values. This code should no
;            longer be needed.
;
;            Added PrintTimings to time the sparse matrix multiply
;            and the removing outliers code.
;
;       02 Nov 2011: 
;            Replaced the SMM call with the new sparse_matrix_mult
;            function.
;
;       01 Nov 2011: 
;            Created a built-in version of "reshape" which doesn't
;            copy any metadata. Removed the "reshape" that was in
;            this file.
;
;       31 Oct 2011: 
;            Replaced "return(tointeger(1))" with just "return(1)"
;            Removed SMM and squeeze, since they don't seemed to be
;            used by anybody.
;
;       14 Oct 2011:
;            Removed references to $ESMFBINDIR. This should be on 
;            user's search path. Also, executable had wrong name,
;            changed to "ESMF_RegridWeightGen".
;======================================================================
; This is the modified version of copy_VarAtts originaly developed by
; Dennis Shea.
;======================================================================
undef("copy_VarAtts_Except")
procedure copy_VarAtts_Except(var_from,var_to, Except [*]:string)    
local att_names, i
begin                                       
    att_names =getvaratts(var_from);
    if(.not.all(ismissing(att_names)))
        do i = 0,dimsizes(att_names)-1
            if (.not.any(att_names(i).eq.Except)) then
                if (isatt(var_to,att_names(i))) then
                    delete(var_to@$att_names(i)$)  ; var_from att may be diff size/type
                end if
                var_to@$att_names(i)$ = var_from@$att_names(i)$
            end if
        end do
    end if
end     ; of copy_VarAtts_Except

;======================================================================
; This function receives a set of variable names and checks 
; if their units attribute contains north or east
; If it contains:
;           north, it returns latitude
;           east, it returns longitude
; otherwise it returns unknown
;======================================================================
undef("isLatorLon")
function isLatorLon(fid,VarNames[*]:string)
begin
    NumNames=dimsizes(VarNames)
    OutPut=new(NumNames,"string","No_FillValue")
    do i=0,NumNames-1
        if (isfilevar(fid,VarNames(i))) then
            if (isfilevaratt(fid,VarNames(i),"units")) then
                if( isStrSubset(str_lower(fid->$VarNames(i)$@units),"north") ) then
                    OutPut(i)="latitude"
                else if ( isStrSubset(str_lower(fid->$VarNames(i)$@units),"east") ) then
                    OutPut(i)="longitude"
                else
                    OutPut(i)="unknown"
                end if
                end if
            else
                OutPut(i)="unknown"
            end if
        else
            OutPut="unknown"
        end if
    end do
    return(OutPut)
end
;======================================================================
; This function change the type of a variable to the given type.
; sometime it is needed to change to either float or double. But
; which type is only know at the runtime. So, this tool will become handy.
; It will also copies the attributes
;======================================================================
undef("totypeof")
function totypeof(inVar,outType)
begin
    Err="OK"
    if (outType.eq."double") then
        outVar=todouble(inVar)
    else if (outType.eq."float") then
        outVar=tofloat(inVar)
    else if (outType.eq."integer") then
        outVar=tointeger(inVar)
    else if (outType.eq."int64") then
        outVar=toint64(inVar)
    else if (outType.eq."uint64") then
        outVar=touint64(inVar)
    else if (outType.eq."long") then
        outVar=tolong(inVar)
    else if (outType.eq."ulong") then
        outVar=toulong(inVar)
    else if (outType.eq."uint") then
        outVar=touint(inVar)
    else if (outType.eq."short") then
        outVar=toshort(inVar)
    else if (outType.eq."ushort") then
        outVar=toushort(inVar)
    else if (outType.eq."byte") then
        outVar=tobyte(inVar)
    else if (outType.eq."ubyte") then
        outVar=toubyte(inVar)
    else if (outType.eq."string") then
        outVar=tostring(inVar)
    else if (outType.eq."character") then
        outVar=tocharacter(inVar)
    else
        print("conversion to the provided type is not supported.")
        return(0)
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    copy_VarAtts_Except(inVar,outVar,"_FillValue")
    return(outVar)
end     ; of totypeof(...)

;======================================================================
; This function Rotates the given input.
;======================================================================
undef("Rotate2D")
function Rotate2D(lat:snumeric,lon:snumeric,Rot)
begin
    if ( any(dimsizes(lat).ne.dimsizes(lon) ) ) then
        print("lat and lon must have same dimension.")
        return(1)
    end if
    
    dime=dimsizes(lat)
    
    tmplat=ndtooned(lat)
    tmplon=ndtooned(lon)
    d2r=atan(1)/45.0;
    tmpPoints=new( (/2,dimsizes(tmplat)/),typeof(lat) )
    tmpPoints(0,:)=tmplon(:);
    tmpPoints(1,:)=tmplat(:);
    RotateMat=(/ (/cos(Rot*d2r), -sin(Rot*d2r)/),(/sin(Rot*d2r), cos(Rot*d2r)/) /)
    tmpPoints=RotateMat#tmpPoints
    lat=onedtond(tmpPoints(1,:),dime)
    lon=onedtond(tmpPoints(0,:),dime)
    return(0)
end     ; of Rotate2D(...)

;======================================================================
; This functions calculates the mirror of p1
; respect to po
;======================================================================
undef("mirrorP2P")
function mirrorP2P(p1,po)
begin
    dVec=p1-po
    MirrorP=po-dVec
    return(MirrorP)
end     ; of mirrorP2P(...)

;======================================================================
; This function will check:
; (1) if the Attname attribute is defined
; (2) if the Attname attribute is a logical attribute
; (3) if the Attname is True
; It will return true only if all above conditions are satisfied.
;======================================================================
undef("isatt_logical_true")
function isatt_logical_true(Opt[1]:logical,AttName[1]:string)
begin
    if (isatt(Opt,AttName) ) then
        if (islogical(Opt@$AttName$)) then
            return(Opt@$AttName$)
        else 
            return(False)
        end if
    else
        return(False)
    end if
end     ; of isatt_logical_true(...)

;======================================================================
; This function will check:
; (1) if Opt is a logical variable.
; (2) if Opt is set to true
; (3) if the Attname attribute of Opt is defined
; (4) if the Attname attribute of Opt is a logical attribute
; (5) if the Attname is True
; It will return true only if all above conditions are satisfied.
;======================================================================
undef("isbothvaratt_logical_true")
function isbothvaratt_logical_true(Opt[1]:logical,AttName[1]:string)
begin
    if (islogical(Opt)) then
        if (Opt) then
            return(isatt_logical_true(Opt,AttName))
        else 
            return(False)
        end if
    else
        return(False)
    end if
end     ; of isbothvaratt_logical_true(...)

;======================================================================
;
; This procedure is used within Curvilinear2SCRIP function. Given a
; structured grid, it looks for the four corners surrounding each
; node. It returns the center of the cells for the corners.
;
;======================================================================
undef("findCorners")
procedure findCorners(lat2d,lon2d,grid_corner_lat,grid_corner_lon)
begin
    dimgrid=dimsizes(lat2d)
    nlat=dimgrid(0)
    nlon=dimgrid(1)
    
    ; Extending the lat/lon grid (needed to calculate the corners at the boundaries)
    Extlat2d=new( (/nlat+2, nlon+2/),typeof(lat2d))
    Extlon2d=new( (/nlat+2, nlon+2/),typeof(lon2d))
    Extlat2d(1:nlat,1:nlon)=(/lat2d/)
    Extlon2d(1:nlat,1:nlon)=(/lon2d/)
    
    Extlat2d(0,1:nlon)=mirrorP2P(lat2d(1,:),lat2d(0,:))
    Extlat2d(nlat+1,1:nlon)=mirrorP2P(lat2d(nlat-2,:),lat2d(nlat-1,:))
    Extlat2d(1:nlat,0)=mirrorP2P(lat2d(:,1),lat2d(:,0))
    Extlat2d(1:nlat,nlon+1)=mirrorP2P(lat2d(:,nlon-2),lat2d(:,nlon-1))
    Extlat2d(0,0)=mirrorP2P(lat2d(1,1),lat2d(0,0))
    Extlat2d(nlat+1,nlon+1)=mirrorP2P(lat2d(nlat-2,nlon-2),lat2d(nlat-1,nlon-1))
    Extlat2d(0,nlon+1)=mirrorP2P(lat2d(1,nlon-2),lat2d(0,nlon-1))
    Extlat2d(nlat+1,0)=mirrorP2P(lat2d(nlat-2,1),lat2d(nlat-1,0))

    Extlon2d(0,1:nlon)=mirrorP2P(lon2d(1,:),lon2d(0,:))
    Extlon2d(nlat+1,1:nlon)=mirrorP2P(lon2d(nlat-2,:),lon2d(nlat-1,:))
    Extlon2d(1:nlat,0)=mirrorP2P(lon2d(:,1),lon2d(:,0))
    Extlon2d(1:nlat,nlon+1)=mirrorP2P(lon2d(:,nlon-2),lon2d(:,nlon-1))
    Extlon2d(0,0)=mirrorP2P(lon2d(1,1),lon2d(0,0))
    Extlon2d(nlat+1,nlon+1)=mirrorP2P(lon2d(nlat-2,nlon-2),lon2d(nlat-1,nlon-1))
    Extlon2d(0,nlon+1)=mirrorP2P(lon2d(1,nlon-2),lon2d(0,nlon-1))
    Extlon2d(nlat+1,0)=mirrorP2P(lon2d(nlat-2,1),lon2d(nlat-1,0))
    
    ; Calculating the cell center of the extended grid,
    ; which would be the corner coordinates for the original grid.
    tmp=Extlat2d(:,1:nlon+1)+Extlat2d(:,0:nlon)
    ExtGridCenter_lat=ndtooned((tmp(1:nlat+1,:)+tmp(0:nlat,:))*0.25)    
                                                            ; Multiplying by 0.25
                                                            ; instead of dividing by 4.
                                                            ; Multiplication is cheaper
    tmp=Extlon2d(:,1:nlon+1)+Extlon2d(:,0:nlon)             ; than the division.
    ExtGridCenter_lon=ndtooned((tmp(1:nlat+1,:)+tmp(0:nlat,:))*0.25)
    delete(tmp)
    ; Now extracting the grid Cell Corners
    ii=ndtooned(conform_dims((/nlat,nlon/),ispan(0,nlon-1,1),1))
    jj=ndtooned(conform_dims((/nlat,nlon/),ispan(0,nlat-1,1),0))
    grid_corner_lat(:,0)=ExtGridCenter_lat(jj*(nlon+1)+ii)
    grid_corner_lat(:,1)=ExtGridCenter_lat(jj*(nlon+1)+(ii+1))
    grid_corner_lat(:,2)=ExtGridCenter_lat((jj+1)*(nlon+1)+(ii+1))
    grid_corner_lat(:,3)=ExtGridCenter_lat((jj+1)*(nlon+1)+ii)
    
    grid_corner_lon(:,0)=ExtGridCenter_lon(jj*(nlon+1)+ii)
    grid_corner_lon(:,1)=ExtGridCenter_lon(jj*(nlon+1)+(ii+1))
    grid_corner_lon(:,2)=ExtGridCenter_lon((jj+1)*(nlon+1)+(ii+1))
    grid_corner_lon(:,3)=ExtGridCenter_lon((jj+1)*(nlon+1)+ii)

end     ; of findCorners(...)

;======================================================================
; This functions Generates an ESMF file for an unstructured grid.
; However, it is sometimes better to even store logically rectangular and structured grid
; as unstructured grid. (Refer to TriPolar - TGrid)
;======================================================================
undef("Unstructured2ESMF")
function Unstructured2ESMF(FName[1]:string,inPutFName[1]:string,inLat,inLon,inNodeMask,Opt[1]:logical)
begin
    ; Checking if the user has requested the unstructured grid
    if ( .not.(isbothvaratt_logical_true(Opt,"ForceUnstructured")) ) then
        print("Can not use this function for Structured grid.")
        return(1)
    end if
    
    ; Checking if the file already exists
    if (isfilepresent(FName)) then
        if (.not.isbothvaratt_logical_true(Opt,"OverWrite")) then
            print("The '" + FName + "' file already exists.")
            return(1)
        else
            print("removing an existing file...")
            if (isbothvaratt_logical_true(Opt,"ForceOverWrite")) then
                system("rm -rf "+FName)
            else
                system("rm -rfi "+FName)
            end if
        end if
    end if

    lat=ndtooned(inLat)
    lon=ndtooned(inLon)
    NodeMask=ndtooned(inNodeMask)
    
    if (dimsizes(lat).ne.dimsizes(lon)) then
        print("Latitude and longitude must have the same number of elements.")
        return(1)
    end if
    if (dimsizes(NodeMask).ne.dimsizes(lat)) then
        print("Mask must have the same number of elements as lat and lon.")
        return(1)
    end if
    
    ; Triangulating and getting the element connectivity
    print("Triangulating the data ...")
    ElementVertices=csstri(lat,lon)
    ElementVertices_Dim=dimsizes(ElementVertices)
    print("Total number of elements created: "+ElementVertices_Dim(0))

    ; Creating the file
    fid=addfile(FName,"c")
    setfileoption(fid,"DefineMode",True)

    ; Defining the file attributes
    print("Setting File attributes ...")
    FileAtt=True
    FileAtt@gridType="unstructured"
    FileAtt@Conventions="ESMF"
    FileAtt@Createdby="ESMFRegridingTools.ncl"
    FileAtt@version="0.9"
    FileAtt@inputFile=inPutFName
    FileAtt@timeGenerated=systemfunc("date")
    FileAtt@date_created=FileAtt@timeGenerated
    fileattdef(fid,FileAtt)
    
    ; Defining the ESMF dimensions
    print("Defining Dimensions ...")
	nodeCount = dimsizes(lat)
    elementCount=ElementVertices_Dim(0)
    maxNodePElement=3
    coordDim=2
	;                0            1               2               3
	FDimNames=(/ "nodeCount","elementCount","maxNodePElement","coordDim" /)
	FDimSizes=(/  nodeCount , elementCount , maxNodePElement , coordDim  /)
	FDimUnlim=(/    False   ,    False     ,      False      ,   False   /)
	
	filedimdef(fid,FDimNames,FDimSizes,FDimUnlim)    

	; Defining Variables
    print("Defining Variables and their attributes ...")
	filevardef(fid,"nodeCoords","double",(/ FDimNames(0),FDimNames(3) /))
    filevardef(fid,"elementConn","integer",(/ FDimNames(1), FDimNames(2) /))
    filevardef(fid,"numElementConn","byte",(/ FDimNames(1) /))
    filevardef(fid,"centerCoords","double",(/ FDimNames(1), FDimNames(3) /))
    filevardef(fid,"elementArea","double",(/ FDimNames(1) /))
    filevardef(fid,"elementMask","integer",(/ FDimNames(1) /))
    
    ; Defining the variables unit attribute
    AttNodeCoords= 0
    AttNodeCoords@units="degrees"
    
    AttElementConn=0
    AttElementConn@long_name="Node Indices that define the element connectivity"
    AttElementConn@_FillValue=-1
    
    AttNumElementConn=0
    AttNumElementConn@long_name="Number of nodes per element"
    
    AttCenterCoords=0
    AttCenterCoords@units="degrees"
    
    AttElementArea=0
    AttElementArea@units="radians^2"
    AttElementArea@long_name="area weights"
    
    filevarattdef(fid,"nodeCoords",AttNodeCoords)
    filevarattdef(fid,"elementConn",AttElementConn)
    filevarattdef(fid,"numElementConn",AttNumElementConn)
    filevarattdef(fid,"centerCoords",AttCenterCoords)
    filevarattdef(fid,"elementArea",AttElementArea)
    
    ; Preparing the file to store the values
    setfileoption(fid,"DefineMode",False)
    
    ; Storing Node Coordinates
    print("Storing Node Coordinates ...")
    nodeCoords=new((/nodeCount,coordDim/),"double","No_FillValue")
    nodeCoords(:,0)=lon
    nodeCoords(:,1)=lat
    fid->nodeCoords=(/nodeCoords/)
    
    ; Storing Element Connectivity
    print("Storing Element Connectivity ...")
    fid->elementConn=(/ElementVertices+1/)
    
    ; Storing numElementConn
    print("Storing numElementConn ...")
    numElementConn=new((/elementCount/),"byte","No_FillValue")
    numElementConn=tobyte(3)
    fid->numElementConn=(/numElementConn/)
    

    ; Storing center of each element
    ; But first calculating the centeroids
    print("Storing center of each element ...")
    centerCoords=new((/elementCount,coordDim/),"double","No_FillValue")
    centerCoords(:,0)=dim_avg(reshape(lon(ndtooned(ElementVertices)),(/elementCount,3/)))
    centerCoords(:,1)=dim_avg(reshape(lat(ndtooned(ElementVertices)),(/elementCount,3/)))
    fid->centerCoords=(/centerCoords/)
    
    ; Storing element area
    print("Storing element area ...")
    d2r=atan(1)/45.0;
    elementArea=new(elementCount,"double","No_FillValue")
    elementalLat=new((/elementCount,3/),"double","No_FillValue")
    elementalLon=new((/elementCount,3/),"double","No_FillValue")
    elementalLat=reshape(lat(ndtooned(ElementVertices)),(/elementCount,3/))
    elementalLon=reshape(lon(ndtooned(ElementVertices)),(/elementCount,3/))
    elementArea=gc_tarea(elementalLat,elementalLon)
    delete(elementalLat)
    delete(elementalLon)
    print("Element Area: min:"+min(elementArea)+" max:"+max(elementArea))
    fid->elementArea=(/elementArea/)

    ; Storing the element Mask
    print("Storing element Mask ...")
    elementMask=dim_sum(reshape(NodeMask(ndtooned(ElementVertices)),(/elementCount,3/)))
    elementMask=where(elementMask.eq.3,1,0)
    fid->elementMask=(/elementMask/)
    
    return(0)
end     ; of Unstructured2ESMF(...)
;======================================================================
; This function receives a 2D curvilinear grid and mask and stores
; it in FName NetCDF file based on SCRIP standard.
; lat2d and lon2d must have (nlat,nlot) dimension.
; Opt controls the behavior of the function
; current attribute in Opt are:
; (1) OverWrite [Logical] if true, and if the out file already exists
;     it will erase the file.
; (2) ForceOverWrite [Logical] If set to true, the user is not asked
;     for removing an existing file. If set to false the user permission
;     is required to remove an existing file. This is ineffective if
;     OverWrite is set to False.
; (3) ForceUnstructured [Logical] if set to true, this function will
;     not be executed.
; (4) ForcedCorners [Logical] if set to true, insted of calculating the
;     the corner points for the cells, these values are read from
;     CornerLat and CornerLon attribute of Opt.
;======================================================================
undef("Curvilinear2SCRIP")
function Curvilinear2SCRIP(FName[1]:string,FTitle[1]:string,lat2d[*][*]:numeric,lon2d[*][*]:numeric,mask2d[*][*]:integer, Opt[1]:logical)
begin
    if (isbothvaratt_logical_true(Opt,"ForceUnstructured")) then
        print("Can not use this function for unstructured grid.")
        return(1)
    end if
    ; Checking if the file already exists
    if (isfilepresent(FName)) then
        if (.not.isbothvaratt_logical_true(Opt,"OverWrite")) then
            print("The file '" + FName + "' already exists.")
            return(1)
        else
            print("Removing an existing file ...")
            if (isbothvaratt_logical_true(Opt,"ForceOverWrite")) then
                system("rm -rf "+FName)
            else
                system("rm -rfi "+FName)
            end if
        end if
    end if
    ; Getting the nlat and nlon
    ; remember that nlat must be the first dimension
    ; and the nlot the second dimension of both lat2d
    ; and lon2d; in other word,
    ; dimsizes(lat2d)=dimsizes(lon2d)=(/ nlat, nlon /)
    if ( any(dimsizes(lat2d).ne.dimsizes(lon2d)) ) then
        print("lat2d and lon2d dimension doesn't match.")
        return(1)
    end if
    g_dim_sizes=dimsizes(lat2d)
    if (dimsizes(g_dim_sizes).ne.2) then
        print("Only a 2D logically rectangular grid is accepted by this function.")
        return(1)
    end if
    nlat=g_dim_sizes(0)
    nlon=g_dim_sizes(1)

    ; Creating the file
    print("Creating a new file ...")
    fid=addfile(FName,"c")
    setfileoption(fid,"DefineMode",True)

    ; Defining the file attributes
    print("Setting file attributes ...")
    FileAtt=True
    FileAtt@title=FTitle
    FileAtt@Conventions="SCRIP"
    FileAtt@Createdby="ESMFRegridingTools.ncl"
    FileAtt@date_created=systemfunc("date")
    fileattdef(fid,FileAtt)
    
    ; Defining the SCRIP dimensions
    print("Setting NetCDF dimensions ...")
	grid_size = nlat*nlon   ; This is number of data points (grid nodes)
	grid_corners = 4	
    grid_rank = 2
	
	FDimNames=(/ "grid_size","grid_corners","grid_rank" /)
	FDimSizes=(/ grid_size,grid_corners,grid_rank /)
	FDimUnlim=(/ False,False,False /)
	
	filedimdef(fid,FDimNames,FDimSizes,FDimUnlim)
	
	; Defining Variables
	print("Defining the variables ...")
	filevardef(fid,"grid_dims","integer","grid_rank")
    filevardef(fid,"grid_center_lat","double","grid_size")
    filevardef(fid,"grid_center_lon","double","grid_size")
    filevardef(fid,"grid_imask","integer","grid_size")
    filevardef(fid,"grid_corner_lat","double",(/ "grid_size", "grid_corners" /) )
    filevardef(fid,"grid_corner_lon","double",(/ "grid_size", "grid_corners" /) )
    
    ; Defining the variables unit attribute
    DummyAtt1= 0
    DummyAtt1@units="degrees"
    DummyAtt2= 0
    DummyAtt2@units="unitless"
    filevarattdef(fid,"grid_center_lat",DummyAtt1)
    filevarattdef(fid,"grid_center_lon",DummyAtt1)
    filevarattdef(fid,"grid_imask",DummyAtt2)
    filevarattdef(fid,"grid_corner_lat",DummyAtt1)
    filevarattdef(fid,"grid_corner_lon",DummyAtt1)
    delete(DummyAtt1)
    delete(DummyAtt2)
    
    ; Preparing the file to store the values
    setfileoption(fid,"DefineMode",False)
    
    ; Storing Grid Dims
    print("Storing grid dimensions ...")
    fid->grid_dims=(/ nlon, nlat /)     ; Remember SCRIP is FORTRAN Code.
                                        ; so (nlat,nlon) in NCL is equivalent to
                                        ; (nlon,nlat) in FORTRAN
    
	; Storing Cell Center Lat/Lon
	print("Storing node coordinates ...")
	fid->grid_center_lat=(/ ndtooned(lat2d) /)
	fid->grid_center_lon=(/ ndtooned(lon2d) /)
	
	; Storing Cell Maskes
	print("Storing the mask ...")
	if (grid_size.ne.dimsizes(ndtooned(mask2d))) then
	    print("Mask is not with appropriate sizes.")
	    return(1)
	else
	    fid->grid_imask=(/ tointeger(ndtooned(mask2d)) /)
	end if
	
    ; Storing the grid cell corners
    print("Storing node corners ...")
    ; But first computing them:
    if (isbothvaratt_logical_true(Opt,"ForcedCorners")) then
        print("Corners are provided. Reading the information ...")
        CornerLat=Opt@CornerLat
        CornerLon=Opt@CornerLon
        grid_corner_lat=reshape( CornerLat,(/ grid_size, grid_corners /))
        grid_corner_lon=reshape( CornerLon,(/ grid_size, grid_corners /))
	else
        ; Now extracting the grid Cell Corners
        print("Calculating the corner nodes ...")
        grid_corner_lat=new( (/ grid_size, grid_corners /), typeof(lat2d) )
        grid_corner_lon=new( (/ grid_size, grid_corners /), typeof(lon2d) )
        findCorners(lat2d,lon2d,grid_corner_lat,grid_corner_lon)
    end if
   
    ; Now storing the cell corners
    if (isatt(grid_corner_lat,"_FillValue")) then
        delete(grid_corner_lat@_FillValue)
    end if
    if (isatt(grid_corner_lon,"_FillValue")) then
        delete(grid_corner_lon@_FillValue)
    end if
    fid->grid_corner_lat=(/ todouble(grid_corner_lat) /)
    fid->grid_corner_lon=(/ todouble(grid_corner_lon) /)

	; If the code made it here, it must return OK.
    return(0)
end     ; of Curvilinear2SCRIP(...)

;======================================================================
; This function accept an array of latitudes and longitudes, creates
; a rectilinear grid based on the given input and then stores it as SCRIP File
; This functions make use of Curvilinear2SCRIP(...)
;======================================================================
undef("Rectilinear2SCRIP")
function Rectilinear2SCRIP(FName[1]:string,FTitle[1]:string,lat[*]:double,lon[*]:double, Opt[1]:logical)
begin
    if ( (dimsizes(dimsizes(lat)).ne.1).or.(dimsizes(dimsizes(lon)).ne.1) ) then
        print("lat and lon must be a one dimensional array.")
        return(1)
    end if
    nlat=dimsizes(lat)
    nlon=dimsizes(lon)
    
    ; generating the grid centers
    print("Constructing the 2D grid based on the given coordinates ...")
	grid_center_lat=todouble(conform_dims((/nlat, nlon/),lat,0))
	grid_center_lon=todouble(conform_dims((/nlat, nlon/),lon,1))

    ; Generating the mask
    print("Assuming all nodes are participating ...")
    print("Change the mask accordingly if needed!")
    mask2d=new((/nlat,nlon/),"integer","No_FillValue")
    mask2d=1
    
    ; now converting to SCRIP
    Err=(Curvilinear2SCRIP(FName,FTitle,grid_center_lat,grid_center_lon,mask2d,Opt))

    return(Err)
end     ; of Rectilinear2SCRIP(...)

;======================================================================
; This function Automatically retrieves the grid information from the file
; and converts the grid to a SCRIP file.
; The variable must have an attribute called coordinates which defines where
; the coordinate information are stored.
;======================================================================
undef("Auto2SCRIP")
function Auto2SCRIP(srcFile[1]:string,VarName[1]:string,SCRIPFName[1]:string,FTitle[1]:string,Opt[1]:logical)
begin
    print("Automatically recognizing the grid and converting to SCRIP standard...")
    ; Opening the file if it exists
    if (.not.isfilepresent(srcFile)) then
        print("Can not find "+srcFile+".")
        return(1)
    else
        fid=addfile(srcFile,"r")
    end if
    
    ; Checking if the variable is present in the file
    if (.not.isfilevar(fid,VarName)) then
        print("The "+VarName+" variable does not exist.")
        return(1)
    else
        VarData=fid->$VarName$
    end if
    
    ; Checking if the coordinates attribute is defined for the variable.
    ; The coordinates can be defined using four methods
    ; (1) having the coordinates stored in a separate variable, but the name of those
    ;     variables are defined in the coordinates attribute for the variable.
    ;     coordType="variable"
    ; (2) the variable have an attribute called lat and lon
    ;     coordType="lat/lan"
    ; (3) the Variable have an attribute called lat2d and lon2d
    ;     coordType="lat2d/lon2d"
    ; (4) having a named dimension where its units attribute is has 
    ;     either "north" or "east" in it
    ; (5) having an attributes where its units attribute is set to either
    ;     "north" or "east"
    
    coordType="unknown"
    if( all(iscoord(VarData,(/"lat","lon"/))) ) then
        coordType="lat/lon"
        LonDimName="lon"
        LatDimName="lat"
    else 
        ; checking if the coordinate information can be found in the attributes.
        isLatFound=False
        isLonFound=False        
        attNames=getvaratts(VarData)
        NumAtt=dimsizes(attNames)
        do i=0,NumAtt-1
            ; Checking if the coordinate attribute is set
            if( isStrSubset(str_lower(attNames(i)),"coordinate") ) then
                print("coordinate attribute found")
                coordType="variable"
                CoordAtt=VarData@$attNames(i)$
                nCoords=str_fields_count(CoordAtt," ")
                if (nCoords.lt.2) then
                    print("At least two coordinates must be present")
                    return(1)
                else
                    VarNamesInCoordAtt=new(nCoords,"string","No_FillValue")
                    do j=0,nCoords-1
                        VarNamesInCoordAtt(j)=str_get_field(CoordAtt,j+1," ")                        
                    end do
                    ResultIsLatorLon=isLatorLon(fid,VarNamesInCoordAtt)
                    LatVarInd=ind(ResultIsLatorLon.eq."latitude")
                    LonVarInd=ind(ResultIsLatorLon.eq."longitude")
                    if (.not.ismissing(LatVarInd)) then
                        isLatFound=True
                        LatVarName=VarNamesInCoordAtt(LatVarInd)
                    end if
                    if (.not.ismissing(LonVarInd)) then
                        isLonFound=True
                        LonVarName=VarNamesInCoordAtt(LonVarInd)
                    end if
                    if(.not.(isLonFound.and.isLatFound)) then
                        print("Can not locate coordinates. Sorry can't help!")
                        return(1)
                    end if
                end if
            end if
        end do  ; end of checking if the coordinate information can be found in the attributes.
        
        ; checking if there is any coordinate dimension
        if (coordType.eq."unknown")
            isLatFound=False
            isLonFound=False  
            nDim=dimsizes(dimsizes(VarData))
            do i=0,nDim-1
                if(isdimnamed(VarData,i)) then
                    tmpDimName=VarData!i
                    if (iscoord(VarData,tmpDimName)) then
                        if(isatt(VarData&$tmpDimName$,"units")) then
                            tmpunits=VarData&$tmpDimName$@units
                            if(isStrSubset(tmpunits,"north")) then
                                isLatFound=True
                                LatDimName=tmpDimName
                            end if
                            if(isStrSubset(tmpunits,"east")) then
                                isLonFound=True
                                LonDimName=tmpDimName
                            end if
                        end if
                    end if
                end if
            end do
            if(isLonFound.and.isLatFound) then
                coordType="lat/lon"
            end if            
        end if      ; end of checking if there is any coordinate dimension
        
        ; checking if there is any attributes containing the coordinates.
        ; it won't work if the attributes are stored as a 1D array.
        if (coordType.eq."unknown") 
            isLatFound=False
            isLonFound=False  
            do i=0,NumAtt-1
                tmpAtt=VarData@$attNames(i)$
                if(isatt(tmpAtt,"units")) then
                    print("made it here")
                    tmpunits=tmpAtt@units
                    if(isStrSubset(tmpunits,"north")) then
                        isLatFound=True
                        LatAttName=attNames(i)
                    end if
                    if(isStrSubset(tmpunits,"east")) then
                        isLonFound=True
                        LonAttName=attNames(i)
                    end if                
                end if
            end do
            if(isLonFound.and.isLatFound) then
                coordType="lat2d/lon2d"
            end if 
        end if      ; end of checking if there is any attributes containing the coordinates.
        if(.not.(isLonFound.and.isLatFound)) then
            print("Can not locate coordinates. Sorry can't help!")
            return(1)
        end if
    end if
    

    ; getting the coordinate variable names and coordinate data
    if (coordType.eq."variable")    
        if(isfilevar(fid,LatVarName)) then
            print("Retrieving latitude coordinates from "+LatVarName+" variable.")
            LatData=fid->$LatVarName$
        else
            print("Can't read latitude information.")
            return(1)
        end if
        if(isfilevar(fid,LonVarName)) then
            print("Retrieving longitude coordinates from "+LonVarName+" variable.")
            LonData=fid->$LonVarName$
        else
            print("Can't read longitude information.")
            return(1)
        end if

    else if (coordType.eq."lat/lon") then
        nCoords=dimsizes(VarData)
        print("Reading coordinate information from "+LonDimName+" and "+LatDimName+" dimensions...")
        LonData=VarData&$LonDimName$
        LatData=VarData&$LatDimName$
    else ; at this point it means coordType=="lat2d/lon2d"
        print("Reading coordinate information from "+LonAttName+" and "+LatAttName+" Attributes...")
        LonData=VarData@$LonAttName$
        LatData=VarData@$LatAttName$
        if ((dimsizes(dimsizes(LonData)).ne.2).or.(dimsizes(dimsizes(LatData)))) then
            print((LonAttName+" and "+LatAttName+" must be 2D."))
            return(1)
        end if
    end if
    end if  ; end of getting coordinate variables names and data
            ; at this point lat/lon data is stored in LatData and LonData
        
    if (.not.all(dimsizes(LonData).eq.dimsizes(LatData))) then
        print("latitude and longitude must have the same number of dimensions.")
        return(1)
    end if
    
    CoordDim=dimsizes(dimsizes(LonData))
    if ((CoordDim.eq.1).and.(coordType.eq."lat/lon")) then
        print("Rectilinear Coordinate found...")
        nlat=dimsizes(LatData)
        nlon=dimsizes(LonData)
        ; generating the grid centers
    	grid_center_lat=todouble(conform_dims((/nlat, nlon/),LatData,0))
	    grid_center_lon=todouble(conform_dims((/nlat, nlon/),LonData,1))
    else if ((CoordDim.eq.2).and.((coordType.eq."lat2d/lon2d").or.(coordType.eq."variable"))) then
        print("Curvilinear Grid is found...")
        grid_center_lat=todouble(LatData)
        grid_center_lon=todouble(LonData)
        delete(LatData)
        delete(LonData)
    else 
        print("Can't help with recognizing the coordinates.")
        return(1)
    end if
    end if
    
    ; getting Mask
    if (isatt(Opt,"Mask2d")) then
        print("Mask information is provided")
        print("Retrieving mask data ...")
        mask2d=tointeger(Opt@Mask2d)
    else
        print("No mask information. Not masking any nodes out")
        mask2d=new(dimsizes(grid_center_lat),"integer","No_FillValue")
        mask2d=1
    end if

    Err=Curvilinear2SCRIP(SCRIPFName,FTitle,grid_center_lat,grid_center_lon,mask2d,Opt)
    return(Err)    
end     ; of Auto2SCRIP(...)

;======================================================================
; This procedure receives the latitude and longitude of the Lower Left and 
; the Upper Right corner of a rectangle and generates a regularly spaced cartesian
; grids based on the lat/lon coordinates. It stores the grid in a NetCDF file
; based on SCRIP standard.
; This functions make use of Curvilinear2SCRIP(...)
;======================================================================
undef("GenBox_with_LLURCorner")
function GenBox_with_LLURCorner(FName[1]:string,FTitle[1]:string,LLCorner[2]:double,URCorner[2]:double,nlat_dlat[1]:numeric,nlon_dlon[1]:numeric,Opt[1]:logical)
begin
    ; Remeber that the existance of the outfile is check in 
    ; Curvilinear2SCRIP(...), which is called here. So, no need to
    ; recheck it.
    
    ; checking the LLCorner and URCorner variables.
    if ( (dimsizes(LLCorner).ne.2).or.(dimsizes(URCorner).ne.2) ) then
        print("LL/URCorner must be a vector with two elements.")
        return(1)
    end if
    if ( (abs(LLCorner(0)).gt.90).or.(abs(URCorner(0)).gt.90) ) then
        print("LL/URCorner must be follow (/lat, lon/) standard.")
        return(1)
    end if

    BoxDiag=URCorner-LLCorner 
    if (Opt.and.(isatt(Opt,"Rotation"))) then
        Rot=tofloat(Opt@Rotation)
        if (Rot.ne.(0.0)) then
            ; alliging the box diagonal to make box sides parallel/perpendicular
            ; to equator.
            if(Rotate2D(BoxDiag(0),BoxDiag(1),-1.0*Rot).ne.0) then
                return(1)
            end if
        end if
    else
        Rot=0.0;
    end if
        
    if ( BoxDiag(0).le.(0.0) ) then
        print("The corner latitudes do not meet the criteria of a bounding box.")
        return(1)
    end if
    if ( BoxDiag(1).le.(0.0) ) then
        print("The corner longitudes do not meet the criteria of a bounding box.")
        return(1)
    end if
        
    ; Generating the latitudes and longitudes
    if ( isbothvaratt_logical_true(Opt,"isSpacing") ) then
        if (Rot.ne.0.0) then
            print("The box is rotated. Spacing may be different than what you meant.")
        end if
        nlat=tointeger(BoxDiag(0)/nlat_dlat+1)
        nlon=tointeger(BoxDiag(1)/nlon_dlon+1)
    else
        nlat=nlat_dlat
        nlon=nlon_dlon
    end if

    lat=fspan(0.0,BoxDiag(0),nlat)
    lon=fspan(0.0,BoxDiag(1),nlon)

	; Generating the Cell Center Lat/Lon
	print("Calculating the coordinates of the inner nodes ...")
	grid_center_lat=todouble(conform_dims((/nlat, nlon/),lat,0))
	grid_center_lon=todouble(conform_dims((/nlat, nlon/),lon,1))
	
	mask2d=tointeger(new((/nlat,nlon/),"integer","No_FillValue"))
	mask2d=1

    ; Rotating back the box, if needed
    if (Rot.ne.(0.0)) then
        if (Rotate2D(grid_center_lat,grid_center_lon,Rot).ne.0) then
            print("Cannot rotate the box.")
            return(1)
        end if    
    end if

    ; Now that the rotation was successful translating it back.
    grid_center_lat=grid_center_lat+LLCorner(0)
    grid_center_lon=grid_center_lon+LLCorner(1)
	Err=(Curvilinear2SCRIP(FName,FTitle,grid_center_lat,grid_center_lon,mask2d,Opt))
    return(Err)
end     ; of GenBox_with_LLURCorner(...)

;======================================================================
; This function generates a SCRIP file describing a uniform box
; based on a given boxcenter and width and height. How the box is 
; rotated can be controled via the options.
; This functions make use of Curvilinear2SCRIP(...) and GenUniformLatLon2SCRIP(...)
;======================================================================
undef("GenBox_with_Center_Dim")
function GenBox_with_Center_Dim(FName[1]:string,FTitle[1]:string,BoxCenter[2]:double,BoxDim[2]:double,nlat_dlat[1]:numeric,nlon_dlon[1]:numeric,Opt[1]:logical)
begin
    Height=BoxDim(0)
    Width=BoxDim(1)
    if (Opt.and.(isatt(Opt,"Rotation"))) then
        Rot=tofloat(Opt@Rotation)
        if (Rot.ne.(0.0)) then
            if (Rotate2D(Height,Width,Rot).ne.0) then
                return(1)
            end if
        end if
    else
        Rot=0.0;
    end if
    URCorner=todouble(BoxCenter+0.5*(/Height, Width/))
    LLCorner=todouble(BoxCenter-0.5*(/Height, Width/))
    Err=GenBox_with_LLURCorner(FName,FTitle,LLCorner,URCorner,nlat_dlat,nlon_dlon,Opt)
    
    return(Err)
end     ; of GenBox_with_Center_Dim(...)

;======================================================================
; Checks if the required dimensions in the SCRIP file are present
;======================================================================
undef("ncl_CheckSCRIPDims")
function ncl_CheckSCRIPDims(fid[1]:file)
begin
    FileDims=getvardims(fid) 
    
    if ( .not.(any(FileDims.eq."grid_rank") ) ) then
        print("grid_rank not defined.")
        return(False)
    else if ( .not.(any(FileDims.eq."grid_size") ) ) then
        print("grid_size not defined.")
        return(False)
    else if ( .not.(any(FileDims.eq."grid_corners") ) ) then
        print("grid_corners not defined.")
        return(False)
    else
        return(True)
    end if
    end if
    end if
end     ; of ncl_CheckSCRIPDims(...)

;======================================================================
; Checks if the required variables in SCRIP file are present
;======================================================================
undef("ncl_CheckSCRIPVars")
function ncl_CheckSCRIPVars(fid[1]:file)
begin
    if ( .not.(isfilevar(fid,"grid_dims")) ) then
        print("grid_dims not defined.")
        return(False)
    else if ( .not.(isfilevar(fid,"grid_center_lat")) ) then
        print("grid_center_lat not defined.")
        return(False)
    else if ( .not.(isfilevar(fid,"grid_center_lon")) ) then
        print("grid_center_lon not defined.")
        return(False)
    else if ( .not.(isfilevar(fid,"grid_imask")) ) then
        print("grid_imask not defined.")
        return(False)
    else if ( .not.(isfilevar(fid,"grid_corner_lat")) ) then
        print("grid_corner_lat not defined.")
        return(False)
    else if ( .not.(isfilevar(fid,"grid_corner_lon")) ) then
        print("grid_corner_lon not defined.")
        return(False)
    else
        return(True)
    end if
    end if
    end if  
    end if
    end if
    end if
end     ; ncl_CheckSCRIPVars(...)

;======================================================================
; Checks if the input filename is following SCRIP standard
;======================================================================
undef("ncl_isSCRIP")
function ncl_isSCRIP(FileName[1]:string)
begin
    if (isfilepresent(FileName)) then
        fid=addfile(FileName,"r")
    else 
        return(False)
    end if
    
    return ncl_CheckSCRIPDims(fid).and.ncl_CheckSCRIPVars(fid)
end     ; of ncl_isSCRIP(...)

;======================================================================
; calls system function ESMF_RegridGenWeight
; Note: ESMFBINDIR environmental variable must be set 
;       prior to calling this procedure
;======================================================================
undef("esmf_remap_weights")
function esmf_remap_weights(srcGrid[1]:string, dstGrid[1]:string, \
                            weight[1]:string, opt[1]:logical)
local opt2, debug, fmsg
begin
  opt2  = opt    ; Make a copy
  debug = isatt_logical_true(opt2,"Debug")

  if(debug) then
    print("Calling ESMF Regrid Weight Generation tool ...")
  end if
    
  if ( .not.(isbothvaratt_logical_true(opt2,"isBothESMF")) ) then
    if ( .not.(isbothvaratt_logical_true(opt2,"isSrcESMF")) ) then
;---Check if the source grid file is a SCRIP file.
      if (.not.(ncl_isSCRIP(srcGrid))) then
        if(debug) then
          print("esmf_remap_weights: source Grid file is not a SCRIP file.")
        end if
        return(1)
      end if
    end if
    if ( .not.(isbothvaratt_logical_true(opt2,"isDstESMF")) ) then
;---Check if the destination grid file is a SCRIP file.
      if (.not.(ncl_isSCRIP(dstGrid))) then
        if(debug) then
          print("esmf_remap_weights: destination grid file is not a SCRIP file.")
        end if
        return(1)
      end if
    end if
  end if

;---Checking if the ESMF regriding weight generator is present.
;;  print("Checking if the ESMF regriding weight generator is present ...")
;;  if (.not.(isfilepresent("ESMF_RegridWeightGen"))) then
;;    print("ESMF regriding weight generator is not present.")
;;    return(1)
;;  end if

;
; All seems ok. Run the ESMF regrid weight generator.
;
; Check the number of processor supported by the system.
;
; The user must have compiled ESMF with parallel features.
;
  if(ismissing(getenv("NumProc"))) then  
    NumProc  = 1
    Executer = " "
  else 
    NumProc = getenv("NumProc")
    Executer = "mpirun -n " +NumProc+" "
  end if
  if(debug) then
    print("esmf_remap_weights: number of processors used: "+NumProc)
  end if

;---Check if the file already exists
  if (isfilepresent(weight)) then
    if (isatt_logical_true(opt2,"OverWrite")) then
      if(debug) then
        print("esmf_remap_weights: the file '" + weight + "' already exists.")
      end if
      return(1)
    else
      if(debug) then
        print("esmf_remap_weights: removing an existing file ...")
      end if
      if (isatt_logical_true(opt2,"ForceOverWrite")) then
        system("rm -rf "+weight)
      else
        system("rm -rfi "+weight)
      end if
    end if
  end if    

;---Build the command (First Pass - non-optional features)
  if (ismissing(getenv("ESMFBINDIR"))) then
    ESMFCMD = Executer+"ESMF_RegridWeightGen " \
              + "--source "+srcGrid \
              + " --destination "+dstGrid \
              + " --weight "+weight
  else 
    ESMFCMD= Executer+"$ESMFBINDIR/ESMF_RegridWeightGen " \
              + "--source "+srcGrid \
              + " --destination "+dstGrid \
              + " --weight "+weight
  end if

;---Build the command (Second Pass - optional features)            
  if (opt2) then
;
; Check if the user has requested a certain method.
; Otherwise, the default will be used.
;
    if (isatt(opt2,"method")) then
      ESMFCMD=ESMFCMD+" --method " + str_lower(opt2@method)
    end if
;
; Check if the user has specified how to handle the poles
; Otherwise, the default will be used.
;
    if (isatt(opt2,"pole")) then
      ESMFCMD=ESMFCMD+" --pole " + str_lower(opt2@pole)
    end if
;        
; Check if the user is specifying that both source and destination
; grids are regional
;
    if (isatt(opt2,"bothRegional")) then
      if (opt2@bothRegional) then
        ESMFCMD=ESMFCMD+" -r"
      end if
    end if
;        
; Check if the user is specifying that the source grid is regional 
; and the destination grid is global
;
    if (isatt(opt2,"srcGridRegional")) then
      if (opt2@srcGridRegional) then
        ESMFCMD=ESMFCMD+" --src_regional"
      end if
    end if
;        
; Check if the user is specifying that the destination grid is 
; regional and the source grid is global
;
    if (isatt(opt2,"dstGridRegional")) then
      if (opt2@dstGridRegional) then
        ESMFCMD=ESMFCMD+" --dst_regional"
      end if
    end if

;---Check if the user specifies that both grids are ESMF			
    if (isatt_logical_true(opt2,"isBothESMF")) then
      ESMFCMD=ESMFCMD+" -t"
    end if
        
;---Checking if the user specifies that source grids are ESMF			
    if (isatt_logical_true(opt2,"isSrcESMF")) then
        ESMFCMD=ESMFCMD+" --src_type ESMF"
    end if

;---Check if the user specifies that source grids are ESMF			
    if (isatt_logical_true(opt2,"isDstESMF")) then
      ESMFCMD=ESMFCMD+" --dst_type ESMF"
    end if

;---Check if the user has requested to print out the full command to be run.
    if (isatt(opt2,"showCMD")) then
      if (opt2@showCMD) then
        print("The following command is about to be executed on the system:")
        print(""+ ESMFCMD)
      end if
    end if
  end if  ; end of building the command (Second Pass - optional features)
		
;---Execute the regridding command
    RegridOut = systemfunc(ESMFCMD)
    print(""+RegridOut)
        
;
; The binary tools performs more sophisticated tests.
; Here it is checked that everything went ok.
;
  if (.not.(any(str_squeeze(RegridOut).eq. \
            "Completed weight generation successfully."))) then
    if(debug) then
      print("esmf_remap_weights: ESMF Offline Weight Generator was not successful.")
    end if
  end if
  if(debug) then
    print("esmf_remap_weights: ESMF Offline Weight Generator was successful.")
  end if    
  return(0)
end     ; of esmf_remap_weights(...)

;======================================================================
; Using the provided weight file, remaps the data 
; from source grid into destination grid
;======================================================================
undef("esmf_remap")
function esmf_remap(srcData:numeric,WFile[1]:string, opt[1]:logical)
local opt2, debug, fmsg, fid, srclat, srclon, dstlat, dstlon, src_grid_dims, \
      src_grid_rank, srcData_dims, srcData_rank, srcData_rgt_dims, \
      srcData_lft_dims, has_leftmost_dims, dst_grid_dims, dst_grid_rank, \
      row, col, v, x, minDataValue, maxDataValue
begin
  opt2  = opt                                  ; Make a copy
  fmsg  = new(1,float)                         ; For error return

;---Check for options
  PrintTimings   = isatt_logical_true(opt2,"PrintTimings")
  DEBUG          = isatt_logical_true(opt2,"Debug")

  if(DEBUG) then
    print("esmf_remap: regridding using interpolation weights ...")
  end if

;---Make sure the weight file is accessible
  if (.not.(isfilepresent(WFile))) then
    print("esmf_remap: weight file doesn't exist or isn't accessible")
    return(fmsg)
  else
    fid = addfile(WFile,"r")
  end if
	
;---Get the lat/lon of source and destination grids
  srclat = fid->yc_a
  srclon = fid->xc_a
  dstlat = fid->yc_b
  dstlon = fid->xc_b
	
;---Check that the source grid "covers" the destination grid 
  if ( .not.( \
       (min(dstlat).ge.min(srclat)).and.  \
       (max(dstlat).le.max(srclat)).and.  \
       (min(dstlon).ge.min(srclon)).and.  \
       (max(dstlon).le.max(srclon))   ) ) then
      print("esmf_remap: warning: destination grid is not completely")
      print("            covered by the source grid.")
  end if
	
;---Get the dimensions and rank of the source grid and input grid.
  src_grid_dims = fid->src_grid_dims(::-1)    ; dimensions are Fortran-based
  src_grid_rank = dimsizes(src_grid_dims)

  srcData_dims = dimsizes(srcData)
  srcData_rank = dimsizes(srcData_dims)

;---Get the dimensions and rank of the destination grid
  dst_grid_dims = fid->dst_grid_dims(::-1)    ; dimensions are Fortran based
  dst_grid_rank = dimsizes(dst_grid_dims)	

;---Check dimensions of srcData
  if(src_grid_rank.gt.srcData_rank) then
    print("esmf_remap: error: the rank of the source dimensions on the")
    print("            weight file is more than the rank of the data")
    print("            to be regridded. Can't continue.")
    return(fmsg)
  end if

  if(DEBUG) then
;---Source grid info
    print("esmf_remap: Source Grid:")
    str = "            dimensions:"
    do i=0,src_grid_rank-1
      str = str + " " + src_grid_dims(i)
    end do
    print("" + str)
    print("            latitude min/max: " + min(srclat) + "/" + max(srclat))
    print("            longitude min/max:" + min(srclon) + "/" + max(srclon))
;---Destination grid info
    print("esmf_remap: Destination Grid:")
    str = "            dimensions:"
    do i=0,dst_grid_rank-1
      str = str + " " + dst_grid_dims(i)
    end do
    print("" + str)
    print("            latitude min/max: " + min(dstlat) + "/" + max(dstlat))
    print("            longitude min/max:" + min(dstlon) + "/" + max(dstlon))
  end if

;---Check that the rightmost dimensions are the same.
  if(srcData_rank.gt.src_grid_rank) then
    srcData_lft_ndims = srcData_rank - src_grid_rank
    srcData_rgt_dims  = srcData_dims(srcData_lft_ndims:)
    srcData_lft_dims  = srcData_dims(0:srcData_lft_ndims-1)
    if(.not.all(srcData_rgt_dims.eq.src_grid_dims)) then
      print("esmf_remap: error: input or source data does not have proper rightmost dimensions.")
      return(fmsg)
    end if
  else
    srcData_rgt_dims  = srcData_dims
    srcData_lft_ndims = 0
  end if

;
; Retrieve the interpolation weights. They are stored as a 
; sparse matrix in a coordinate list format
;
  if(DEBUG) then
    print("esmf_remap: retrieving interpolation weights ...")
  end if

  row = fid->row
  col = fid->col
  v   = fid->S
    
;---Check srcData dimensions
  if ( max(col).gt.product(srcData_rgt_dims) ) then
    print("esmf_remap: error: source data does not have proper dimensions.")
    return(fmsg)
  end if
    
;
; We have to convert the src grid to 1D no matter what.
; However, we have to determine if there are any leftmost 
; dimensions to account for.
;
  if (srcData_rank.gt.src_grid_rank) then
    src_grid_dims_tmp                        = new(srcData_lft_ndims+1,long)
    src_grid_dims_tmp(0:srcData_lft_ndims-1) = srcData_lft_dims
    src_grid_dims_tmp(srcData_lft_ndims)     = product(srcData_rgt_dims)
    x = reshape(srcData,src_grid_dims_tmp)
  else
    x = reshape(srcData,product(srcData_dims))
  end if

;
; Note from Mohammad:
;   Currently there is a problem with ESMF files.
;   once it is fixed by ESMF people This can be reinstated.
;  if( max(row).gt.product(dst_grid_dims) ) then  
;    print("esmf_remap: error: weight file has internal mistmatched data or is corrupted.")
;    return(fmsg)
;  end if
      
  if(DEBUG)
    print("esmf_remap: calling sparse_matrix_mult to apply weights...")
  end if
  if(PrintTimings) then
    start_time = get_cpu_time()
  end if

;
; We have to always treat the output grid as N x 1
; (or M x N x 1 if we have leftmost dimensions) 
; because the input grid has to be 1D.
;
; If dst_grid_dims is 1, then we have to set N=max(row).
;
  if(srcData_rank.gt.src_grid_rank) then
    dst_grid_dims_in = new(srcData_lft_ndims+1,long)
    dst_grid_dims_in(0:srcData_lft_ndims-1) = srcData_lft_dims
    if(dst_grid_rank.eq.1) then
      dst_grid_dims_in(srcData_lft_ndims) = max(row)
    else
      dst_grid_dims_in(srcData_lft_ndims) = product(dst_grid_dims)
    end if
  else
    if(dst_grid_rank.eq.1) then
      dst_grid_dims_in = max(row)
    else
      dst_grid_dims_in = product(dst_grid_dims)
    end if
  end if
;
; We need to reshape our (M) x N x 1 grid if the output dimensions
; are not 1D.
;
  if(dst_grid_rank.gt.1) then
    dst_grid_dims_out = new(srcData_lft_ndims+dst_grid_rank,long)
    if(srcData_rank.gt.src_grid_rank) then
      dst_grid_dims_out(0:srcData_lft_ndims-1) = srcData_lft_dims
    end if
    dst_grid_dims_out(srcData_lft_ndims:) = dst_grid_dims
    dstData = reshape(sparse_matrix_mult(row-1,col-1,v,x,dst_grid_dims_in),\
                      dst_grid_dims_out)
  else
    dstData = sparse_matrix_mult(row-1,col-1,v,x,dst_grid_dims_in)
  end if

  if(PrintTimings) then
    end_time = get_cpu_time()
    print("esmf_remap: sparse_matrix_mult took " + \
           (end_time-start_time) + " seconds.")
  end if

;---Make sure dstData has a missing value
  if(isatt(srcData,"_FillValue")) then
    dstData@_FillValue = srcData@_FillValue
  else
    dstData@_FillValue = default_fillvalue(typeof(dstData))
  end if

  if(DEBUG) then
    print("esmf_remap: dstData")
    str = "            Dimensions:"
    nd = dimsizes(dstData)
    do i=0,dimsizes(nd)-1
      str = str + " " + nd(i)
    end do
    print("" + str)
    print("            minSrcData: " + min(srcData))
    print("            maxSrcData: " + max(srcData))
    print("            minDstData: " + min(dstData))
    print("            maxDstData: " + max(dstData))
  end if

;
; Copy some attributes.
; Note: The user is responsible for attributes in general.
;
  if (isatt(srcData,"units")) then
    dstData@units=srcData@units
  end if
  if (isatt(srcData,"long_name")) then
    dstData@long_name="remapped "+srcData@long_name
  end if
  if (isatt(srcData,"short_name")) then
    dstData@short_name=srcData@short_name
  end if
	
  return(dstData)
end     ; of esmf_remap(...)

;======================================================================
; This function will retrieve the latitude coordinate of a grid
; from a SCRIP formatted file.
;======================================================================
undef("retrieve_SCRIP_lat")
function retrieve_SCRIP_lat(fileName[1]:string)
begin
    if (.not.isfilepresent(fileName)) then
        print("The requested file is not present")
        return(0.0)
    end if
    
    fid=addfile(fileName,"r")
    grid_dims=fid->grid_dims;
    grid_center_lat= reshape( fid->grid_center_lat , grid_dims(::-1))
    grid_center_lat@units="degrees_north"
    return( todouble(grid_center_lat) )
end     ; of retrieve_SCRIP_lat

;======================================================================
; This function will retrieve the longitude coordinate of a grid
; from a SCRIP formatted file.
;======================================================================
undef("retrieve_SCRIP_lon")
function retrieve_SCRIP_lon(fileName[1]:string)
begin
    if (.not.isfilepresent(fileName)) then
        print("The requested file is not present")
        return(0.0)
    end if
    
    fid=addfile(fileName,"r")
    grid_dims=fid->grid_dims;
    grid_center_lon= reshape( fid->grid_center_lon , grid_dims(::-1))
    grid_center_lon@units="degrees_east"
    return( todouble(grid_center_lon) )
end     ; of retrieve_SCRIP_lon

;======================================================================
undef("retrieve_dstGrid_lat")
function retrieve_dstGrid_lat(fileName[1]:string)
begin
    print("Retrieving Destination Grid latitudes from the weight file ...")
    fid=addfile(fileName,"r")
    dst_grid_dims=fid->dst_grid_dims
    Err=" "
    lat=reshape( fid->yc_b , dst_grid_dims(::-1))
    return(lat)
    
end     ; of retrieve_dstGrid_lat

;======================================================================
undef("retrieve_dstGrid_lon")
function retrieve_dstGrid_lon(fileName[1]:string)
begin
    print("Retrieving Destination Grid longitudes from the weight file ...")
    fid=addfile(fileName,"r")
    dst_grid_dims=fid->dst_grid_dims
    Err=" "
    lon=reshape( fid->xc_b , dst_grid_dims(::-1))
    return(lon)
    
end     ; of retrieve_dstGrid_lon

;======================================================================
undef("retrieve_srcGrid_lat")
function retrieve_srcGrid_lat(fileName[1]:string)
begin
    print("Retrieving Source Grid latitudes from the weight file ...")
    fid=addfile(fileName,"r")
    src_grid_dims=fid->src_grid_dims
    Err=" "
    lat=reshape( fid->yc_a , src_grid_dims(::-1))
    return(lat)
    
end     ; of retrieve_srcGrid_lat

;======================================================================
undef("retrieve_srcGrid_lon")
function retrieve_srcGrid_lon(fileName[1]:string)
begin
    print("Retrieving Source Grid longitudes from the weight file ...")
    fid=addfile(fileName,"r")
    src_grid_dims=fid->src_grid_dims
    Err=" "
    lon=reshape( fid->xc_a , src_grid_dims(::-1))
    return(lon)
    
end     ; of retrieve_srcGrid_lon

