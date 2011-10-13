
; The following functions and procedures are written by:
;        Mohammad Abouali, maboualiedu@gmail.com
;              May 22nd, July 30th, 2011
;
;        http://sites.google.com/site/maboualihome/

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
; This is the modified version of copy_VarAtts originaly developed by Dennis Shea.
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
;===================================================================================================
; This function accept an array with any dimensionality, 
; and outputs the same array with the new requested dimension.
; Note:
;     - Number of elements can not change.
;     - Rearranging is done based on the C/NCL array indexing.
undef("reshape")
function reshape(inVar,newdimension [*]:integer)
begin
    if ( product(dimsizes(inVar)).ne.product(newdimension) ) then
        print("The number of element can not be changed.")
        return(inVar)
    end if
    tmp=ndtooned(inVar)
    outVar=onedtond(tmp,newdimension)    
    copy_VarAtts(inVar,outVar)
    
    ; since it is rearranged, the coordinate may not be meaningful anymore.
    if (isatt(outVar,"coordinates") )then
        delete(outVar@coordinates)
    end if
    if (isatt(outVar,"Coordinates") )then
        delete(outVar@Coordinates)
    end if    
    return(outVar)
end     ; of reshape(...)

;===================================================================================================
; This function receives a set of variable names and checks 
; if their units attribute contains north or east
; If it contains:
;           north, it returns latitude
;           east, it returns longitude
; otherwise it returns unknown
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
;===================================================================================================
; This function change the type of a variable to the given type.
; sometime it is needed to change to either float or double. But
; which type is only know at the runtime. So, this tool will become handy.
; It will also copies the attributes
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

;===================================================================================================
; This function multiplies a sparse matrix stored in coordinate list,
; i.e. (row,col,value) by a vector and adds it to the output vector.
; or: y <= Ax+y
;
; NOTE: Try to use SMMulFast instead of this.
undef("SMM")
function SMM(row[*]:integer,col[*]:integer,v[*]:double,x:double,y:double)
begin
    Err="OK"
    n_s=dimsizes(row)
    if ( (n_s.ne.dimsizes(col)).or.(n_s.ne.dimsizes(v)) ) then
        print("Row, Col, and/or V dimension do not match")
        return(tointeger(1))
    end if
    n_x=dimsizes(x);
    n_y=dimsizes(y);
    if ( (dimsizes(n_x).gt.2).or.(dimsizes(n_y).gt.2) ) then
        print("x and/or y can be either a vector or 2 dimensional variable.")
        return(tointeger(1))
    end if
    
    if ( max(col).gt.n_x(0) ) then
        print("Sparse matrix is indexing elements that do not exist in x.")
        return(tointeger(1))
    end if
    
    if ( max(row).gt.n_y(0) ) then
        print("Sparse matrix is indexing elements that do not exist in y.")
        return(tointeger(1))
    end if
    
    if (dimsizes(n_x).ne.dimsizes(n_y)) then
        print("x and y must be the same dimension.")
        return(tointeger(1))
    end if
    
    if (dimsizes(n_x).eq.2) then
        if( n_x(1).ne.n_y(1) ) then
            print("the second dimension of both x and y must be the same")
            return(tointeger(1))
        end if
        nDataField=n_x(1)
    else
        nDataField=1
    end if
    
    xReshaped=reshape(x,(/n_x(0), nDataField/))
    yReshaped=todouble(reshape(y,(/n_y(0), nDataField/)))
    
    ; The core of computation; so far it was just error checking and reshaping
    do i=0,n_s-1
        yReshaped(row(i)-1,:)=yReshaped(row(i)-1,:)+xReshaped(col(i)-1,:)*v(i)
    end do

    y=totypeof(reshape(yReshaped,n_y),typeof(y))
    return(tointeger(0))
end     ; of SMM(...)

;===================================================================================================
; This function Rotates the given input.
undef("Rotate2D")
function Rotate2D(lat:snumeric,lon:snumeric,Rot)
begin
    if ( any(dimsizes(lat).ne.dimsizes(lon) ) ) then
        print("lat and lon must have same dimension.")
        return(tointeger(1))
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
    return(tointeger(0))
end     ; of Rotate2D(...)

;===================================================================================================
; This functions calculates the mirror of p1
; respect to po
undef("mirrorP2P")
function mirrorP2P(p1,po)
begin
    dVec=p1-po
    MirrorP=po-dVec
    return(MirrorP)
end     ; of mirrorP2P(...)

;===================================================================================================
; This function will check:
; (1) if the Attname attribute is defined
; (2) if the Attname attribute is a logical attribute
; (3) if the Attname is True
; It will return true only if all above conditions are satisfied.
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

;===================================================================================================
; This function will check:
; (1) if Opt is a logical variable.
; (2) if Opt is set to true
; (3) if the Attname attribute of Opt is defined
; (4) if the Attname attribute of Opt is a logical attribute
; (5) if the Attname is True
; It will return true only if all above conditions are satisfied.
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

;===================================================================================================
; This procedure is used within Curvilinear2SCRIP function. Given a structured grid, it looks
; for the four corners surrounding each node. It returns the center of the cells for the corners.
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

;===================================================================================================
; This functions Generates an ESMF file for an unstructured grid.
; However, it is sometimes better to even store logically rectangular and structured grid
; as unstructured grid. (Refer to TriPolar - TGrid)
undef("Unstructured2ESMF")
function Unstructured2ESMF(FName[1]:string,inPutFName[1]:string,inLat,inLon,inNodeMask,Opt[1]:logical)
begin
    ; Checking if the user has requested the unstructured grid
    if ( .not.(isbothvaratt_logical_true(Opt,"ForceUnstructured")) ) then
        print("Can not use this function for Structured grid.")
        return(tointeger(1))
    end if
    
    ; Checking if the file already exists
    if (isfilepresent(FName)) then
        if (.not.isbothvaratt_logical_true(Opt,"OverWrite")) then
            print("The file already exists.")
            return(tointeger(1))
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
        return(tointeger(1))
    end if
    if (dimsizes(NodeMask).ne.dimsizes(lat)) then
        print("Mask must have the same number of elements as lat and lon.")
        return(tointeger(1))
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
    
    ; Storing Node Cooridnates
    print("Storing Node Cooridnates ...")
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
    
    return(tointeger(0))
end     ; of Unstructured2ESMF(...)
;===================================================================================================
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
undef("Curvilinear2SCRIP")
function Curvilinear2SCRIP(FName[1]:string,FTitle[1]:string,lat2d[*][*]:double,lon2d[*][*]:double,mask2d[*][*]:integer, Opt[1]:logical)
begin
    if (isbothvaratt_logical_true(Opt,"ForceUnstructured")) then
        print("Can not use this function for unstructured grid.")
        return(tointeger(1))
    end if
    ; Checking if the file already exists
    if (isfilepresent(FName)) then
        if (.not.isbothvaratt_logical_true(Opt,"OverWrite")) then
            print("The file already exists.")
            return(tointeger(1))
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
        return(tointeger(1))
    end if
    g_dim_sizes=dimsizes(lat2d)
    if (dimsizes(g_dim_sizes).ne.2) then
        print("Only a 2D logically rectangular grid is accepted by this function.")
        return(tointeger(1))
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
	    return(tointeger(1))
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
    return(tointeger(0))
end     ; of Curvilinear2SCRIP(...)

;===================================================================================================
; This function accept an array of latitudes and longitudes, creates
; a rectilinear grid based on the given input and then stores it as SCRIP File
; This functions make use of Curvilinear2SCRIP(...)
undef("Rectilinear2SCRIP")
function Rectilinear2SCRIP(FName[1]:string,FTitle[1]:string,lat[*]:double,lon[*]:double, Opt[1]:logical)
begin
    if ( (dimsizes(dimsizes(lat)).ne.1).or.(dimsizes(dimsizes(lon)).ne.1) ) then
        print("lat and lon must be a one dimensional array.")
        return(tointeger(1))
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

;===================================================================================================
; This function Automatically retrieves the grid information from the file
; and converts the grid to a SCRIP file.
; The variable must have an attribute called coordinates which defines where
; the coordinate information are stored.
undef("Auto2SCRIP")
function Auto2SCRIP(srcFile[1]:string,VarName[1]:string,SCRIPFName[1]:string,FTitle[1]:string,Opt[1]:logical)
begin
    print("Automatically recognizing the grid and converting to SCRIP standard...")
    ; Opening the file if it exists
    if (.not.isfilepresent(srcFile)) then
        print("Can not find "+srcFile+".")
        return(tointeger(1))
    else
        fid=addfile(srcFile,"r")
    end if
    
    ; Checking if the variable is present in the file
    if (.not.isfilevar(fid,VarName)) then
        print("The "+VarName+" variable does not exists.")
        return(tointeger(1))
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
                    return(tointeger(1))
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
                        return(tointeger(1))
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
            return(tointeger(1))
        end if
    end if
    

    ; getting the coordinate variable names and coordinate data
    if (coordType.eq."variable")    
        if(isfilevar(fid,LatVarName)) then
            print("Retriveing latitude coordinates from "+LatVarName+" variable.")
            LatData=fid->$LatVarName$
        else
            print("Can't read latitude information.")
            return(tointeger(1))
        end if
        if(isfilevar(fid,LonVarName)) then
            print("Retriveing longitude coordinates from "+LonVarName+" variable.")
            LonData=fid->$LonVarName$
        else
            print("Can't read longitude information.")
            return(tointeger(1))
        end if

    else if (coordType.eq."lat/lon") then
        nCoords=dimsizes(VarData)
        print("Reading coordinate informatino from "+LonDimName+" and "+LatDimName+" dimensions...")
        LonData=VarData&$LonDimName$
        LatData=VarData&$LatDimName$
    else ; at this point it means coordType=="lat2d/lon2d"
        print("Reading coordinate information from "+LonAttName+" and "+LatAttName+" Attributes...")
        LonData=VarData@$LonAttName$
        LatData=VarData@$LatAttName$
        if ((dimsizes(dimsizes(LonData)).ne.2).or.(dimsizes(dimsizes(LatData)))) then
            print((LonAttName+" and "+LatAttName+" must be 2D."))
            return(tointeger(1))
        end if
    end if
    end if  ; end of getting coordinate variables names and data
            ; at this point lat/lon data is stored in LatData and LonData
        
    if (.not.all(dimsizes(LonData).eq.dimsizes(LatData))) then
        print("latitude and longitude must have the same number of dimensions.")
        return(tointeger(1))
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
        print("Can't help with the recognizing the coordinates.")
        return(tointeger(1))
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

;===================================================================================================
; This procedure receives the latitude and longitude of the Lower Left and 
; the Upper Right corner of a rectangle and generates a regularly spaced cartesian
; grids based on the lat/lon coordinates. It stores the grid in a NetCDF file
; based on SCRIP standard.
; This functions make use of Curvilinear2SCRIP(...)
undef("GenBox_with_LLURCorner")
function GenBox_with_LLURCorner(FName[1]:string,FTitle[1]:string,LLCorner[2]:double,URCorner[2]:double,nlat_dlat[1]:numeric,nlon_dlon[1]:numeric,Opt[1]:logical)
begin
    ; Remeber that the existance of the outfile is check in 
    ; Curvilinear2SCRIP(...), which is called here. So, no need to
    ; recheck it.
    
    ; checking the LLCorner and URCorner variables.
    if ( (dimsizes(LLCorner).ne.2).or.(dimsizes(URCorner).ne.2) ) then
        print("LL/URCorner must be a vector with two elements.")
        return(tointeger(1))
    end if
    if ( (abs(LLCorner(0)).gt.90).or.(abs(URCorner(0)).gt.90) ) then
        print("LL/URCorner must be follow (/lat, lon/) standard.")
        return(tointeger(1))
    end if

    BoxDiag=URCorner-LLCorner 
    if (Opt.and.(isatt(Opt,"Rotation"))) then
        Rot=tofloat(Opt@Rotation)
        if (Rot.ne.(0.0)) then
            ; alliging the box diagonal to make box sides parallel/perpendicular
            ; to equator.
            if(Rotate2D(BoxDiag(0),BoxDiag(1),-1.0*Rot).ne.0) then
                return(tointeger(1))
            end if
        end if
    else
        Rot=0.0;
    end if
        
    if ( BoxDiag(0).le.(0.0) ) then
        print("The corner latitudes do not meet the criteria of a bounding box.")
        return(tointeger(1))
    end if
    if ( BoxDiag(1).le.(0.0) ) then
        print("The corner longitudes do not meet the criteria of a bounding box.")
        return(tointeger(1))
    end if
        
    ; Generating the latitudes and longitudes
    if ( isbothvaratt_logical_true(Opt,"isSpacing") ) then
        if (Rot.ne.0.0) then
            print("The box is rotated.Spacing may be different than what you meant.")
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
            return(tointeger(1))
        end if    
    end if

    ; Now that the rotation was successfull translating it back.
    grid_center_lat=grid_center_lat+LLCorner(0)
    grid_center_lon=grid_center_lon+LLCorner(1)
	Err=(Curvilinear2SCRIP(FName,FTitle,grid_center_lat,grid_center_lon,mask2d,Opt))
    return(Err)
end     ; of GenBox_with_LLURCorner(...)

;===================================================================================================
; This function generates a SCRIP file describing a uniform box
; based on a given boxcenter and width and height. How the box is 
; rotated can be controled via the options.
; This functions make use of Curvilinear2SCRIP(...) and GenUniformLatLon2SCRIP(...)
undef("GenBox_with_Center_Dim")
function GenBox_with_Center_Dim(FName[1]:string,FTitle[1]:string,BoxCenter[2]:double,BoxDim[2]:double,nlat_dlat[1]:numeric,nlon_dlon[1]:numeric,Opt[1]:logical)
begin
    Height=BoxDim(0)
    Width=BoxDim(1)
    if (Opt.and.(isatt(Opt,"Rotation"))) then
        Rot=tofloat(Opt@Rotation)
        if (Rot.ne.(0.0)) then
            if (Rotate2D(Height,Width,Rot).ne.0) then
                return(tointeger(1))
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

;===================================================================================================
; Checks if the required dimensions in the SCRIP file are present
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

;===================================================================================================
; Checks if the required variables in SCRIP file are present
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

;===================================================================================================
; Checks if the input filename is following SCRIP standard
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

;===================================================================================================
; calls system function ESMF_RegridGenWeight
; Note: ESMFBINDIR environmental variable must be set 
;       prior to calling this procedure
undef("esmf_remap_weights")
function esmf_remap_weights(srcGrid[1]:string, dstGrid[1]:string, weight[1]:string, Opt[1]:logical)
begin
    print("Calling ESMF Regrid Weight Generation tool ...")
    
    if ( .not.(isbothvaratt_logical_true(Opt,"isBothESMF")) ) then
        if ( .not.(isbothvaratt_logical_true(Opt,"isSrcESMF")) ) then
            ; Checking if the source grid file is a SCRIP file.
            if (.not.(ncl_isSCRIP(srcGrid))) then
                print("Srouce Grid file is not a SCRIP file.")
                return(tointeger(1))
            end if
        end if
        if ( .not.(isbothvaratt_logical_true(Opt,"isDstESMF")) ) then
            ; Checking if the destination grid file is a SCRIP file.
            if (.not.(ncl_isSCRIP(dstGrid))) then
                print("Destination Grid file is not a SCRIP file.")
                return(tointeger(1))
            end if
        end if
    end if
    ; Checking if the ESMF regriding weight generator is present.
    print("Checking if the ESMF regriding weight generator is present ...")
    if (.not.(isfilepresent("$ESMFBINDIR/ESMF_regridWeightGen"))) then
        print("ESMF regriding weight generator is not present.")
        return(tointeger(1))
    else 
        print(" ==> OK")
    end if
    ; All seems ok. So, run the ESMF regrid weight generator.

    ; Checking the number of processor supported by the system
    ; The user must have compiled ESMF with parallel features.
    if(ismissing(getenv("NumProc"))) then  
        NumProc=1
        Executer=" "
    else 
        NumProc=getenv("NumProc")
        Executer="mpirun -n " +NumProc+" "
    end if
    print("Number of Processors used: "+NumProc)

    ; Checking if the file already exists
    if (isfilepresent(weight)) then
        if (.not.isbothvaratt_logical_true(Opt,"OverWrite")) then
            print("The file already exists.")
            return(tointeger(1))
        else
            print("Removing an existing file ...")
            if (isbothvaratt_logical_true(Opt,"ForceOverWrite")) then
                system("rm -rf "+weight)
            else
                system("rm -rfi "+weight)
            end if
        end if
    end if    

    
    ; Building the command (First Pass - nonoptional features)
    if (ismissing(getenv("ESMFBINDIR"))) then
        ESMFCMD= Executer+"$NCARG_ROOT/bin/ESMF_regridWeightGen " \
                +"--source "+srcGrid \
                +" --destination "+dstGrid \
                +" --weight "+weight
    else 
        ESMFCMD= Executer+"$ESMFBINDIR/ESMF_regridWeightGen " \
                +"--source "+srcGrid \
                +" --destination "+dstGrid \
                +" --weight "+weight
    end if

    ; Building the command (Second Pass - optional features)            
    if (Opt) then
        ; Checking if the user has requested a certain method
        ; otherwise, the default will be used
        if (isatt(Opt,"method")) then
            ESMFCMD=ESMFCMD+" --method " + str_lower(Opt@method)
        end if
        
        ; Checking if the user has specified how to handle the poles
        ; otherwise, the default will be used.
        if (isatt(Opt,"pole")) then
            ESMFCMD=ESMFCMD+" --pole " + str_lower(Opt@pole)
        end if
        
        ; Checking if the user is specifying that both source and destination
        ; grids are regional
        if (isatt(Opt,"bothRegional")) then
            if (Opt@bothRegional) then
                ESMFCMD=ESMFCMD+" -r"
            end if
        end if
        
        ; Checking if the user is specifying that the source grid is regional 
        ; and the destination grid is global
        if (isatt(Opt,"srcGridRegional")) then
            if (Opt@srcGridRegional) then
                ESMFCMD=ESMFCMD+" --src_regional"
            end if
        end if
        
        ; Checking if the user is specifying that the destination grid is 
        ; regional and the source grid is global
        if (isatt(Opt,"dstGridRegional")) then
            if (Opt@dstGridRegional) then
                ESMFCMD=ESMFCMD+" --dst_regional"
            end if
        end if

        ; Checking if the user specifies that both grids are ESMF			
        if (isatt_logical_true(Opt,"isBothESMF")) then
            ESMFCMD=ESMFCMD+" -t"
        end if
        
        ; Checking if the user specifies that source grids are ESMF			
        if (isatt_logical_true(Opt,"isSrcESMF")) then
            ESMFCMD=ESMFCMD+" --src_type ESMF"
        end if

        ; Checking if the user specifies that source grids are ESMF			
        if (isatt_logical_true(Opt,"isDstESMF")) then
            ESMFCMD=ESMFCMD+" --dst_type ESMF"
        end if

        ; Checking if the user has requested to print out the full command to be run.
        if (isatt(Opt,"showCMD")) then
            if (Opt@showCMD) then
                print("The following command is about to be executed on the system:")
                print(""+ ESMFCMD)
            end if
        end if
    end if  ; end of Building the command (Second Pass - optional features)
		
    ; calling the the system function
    RegridOut=systemfunc(ESMFCMD)
    print(""+RegridOut(:))
        
    ; The binary tools performs more sophisticated tests.
    ; Here it is checked that everything went ok.
    if (.not.(any(str_squeeze(RegridOut).eq."Completed weight generation successfully."))) then
        esmfErr="ESMF Offline Weight Generator was not successfull."
        ; no need to return, this is the last statement anyway.
    end if
    
    return(tointeger(0))
end     ; of esmf_remap_weights(...)

;===================================================================================================
; Using the provided weight file, remaps the data 
; from source grid into destination grid
undef("esmf_remap")
function esmf_remap(srcData:numeric,WFile[1]:string, esmfErr[1]:integer)
begin
    esmfErr=0
    print("Remapping using Interpolation weights ...")

	; Making sure that the Weight file is accessible
	print("Checking the weight file ...")
	if (.not.(isfilepresent(WFile))) then
		print("Weight file does not exists or is not accesible")
		esmfErr=tointeger(1)
		return(0.0)
	else
		fid=addfile(WFile,"r")
	end if
	
	; Checking the coverage
	srclat=fid->yc_a
	srclon=fid->xc_a
	dstlat=fid->yc_b
	dstlon=fid->xc_b
	print("Source Grid:")
	print("Latitude min:"+min(srclat)+" max:"+max(srclat))
	print("Longitude min:"+min(srclon)+" max:"+max(srclon))
	print("Destination Grid:")
	print("Latitude min:"+min(dstlat)+" max:"+max(dstlat))
	print("Longitude min:"+min(dstlon)+" max:"+max(dstlon))
	
	if ( .not.( \
	     (min(dstlat).ge.min(srclat)).and.  \
	     (max(dstlat).le.max(srclat)).and.  \
	     (min(dstlon).ge.min(srclon)).and.  \
	     (max(dstlon).le.max(srclon))   ) ) then
        print("WARNING!!!")
        print("Destination grid is not completely covered by the source grid.")
    end if
    
	
	print("Checking Source Data rank with those given in the weight file ...")
	src_grid_dims=fid->src_grid_dims
	src_rank=dimsizes(src_grid_dims)

	; Checking dimension of srcData
	if (dimsizes(dimsizes(srcData)).ne.(src_rank)) then
		print("Source Data does not have proper dimensions.")
		esmfErr=tointeger(1)
		return(0.0)
	end if
	
	; Reading the dstData sizes
	dst_grid_dims=fid->dst_grid_dims
	dst_rank=dimsizes(dst_grid_dims)	

    ; retrieving the interpolation weigths; 
    ; It is stored as a sparse matrix in a coordinate list format
    print("Retrieving interpolation Weights ...")
    row=tointeger( fid->row )
    col=tointeger( fid->col )
    v=todouble( fid->S )
    
    ; Checking srcData dimensions
    print("Checking Source Data dimensions with those given in the weight file ...")

    if (src_rank.gt.1) then
        if ( .not.(all(dimsizes(srcData).eq.src_grid_dims(::-1))) ) then
            print("Source Data does not have proper dimensions.")
            esmfErr=tointeger(1)
            return(0.0)
        end if
    else ; this means src_rank=1
        if ( max(col).gt.product(dimsizes(srcData)) ) then
            print("Source Data does not have proper dimensions.")
            esmfErr=tointeger(1)
            return(0.0)
        end if
    end if    
    
    ; linearizing the source data
    x=reshape(todouble(srcData),(/product(dimsizes(srcData)),1/))

    ; currently there is a problem with ESMF files.
    ; once it is fixed by ESMF people This can be reinstated.
;    if( max(row).gt.product(dst_grid_dims) ) then  
;        print("Weight file has internal mistmatched data or corrupted.")
;        esmfErr=tointeger(1)
;        return(0.0)
;    end if
    
    if (dst_rank.gt.1) then
        if( any(ismissing(x)) ) then 
            y=new((/product(dst_grid_dims),1/),"double")
        else
            y=new((/product(dst_grid_dims),1/),"double","No_FillValue")
        end if
    else    ; This means dst_rank=1
            ; this else section is only needed currently due to the bug that exists
            ; in ESMF. Once it is corrected that bug, then this else section is not needed.
            ; and the outer can be removed too.
        if( any(ismissing(x)) ) then 
            y=new((/max(row),1/),"double")
        else
            y=new((/max(row),1/),"double","No_FillValue")
        end if        
    end if
    y=0.0;

    print("Interpolating using the weights ...")
    SMMulFast(row,col,v,x,y)
    if (dst_rank.gt.1) then
        dstData=reshape(y,dst_grid_dims(::-1))  ; the dimensions are reversed;
                                                ; It's due to differences between 
                                                ; array indexing in FORTRAN and C.
    else ; this means dst_rank=1
        dstData=ndtooned(y)
    end if

    ; finding data range +-20%
    minDataValue=min(srcData)-0.2d*abs(min(srcData))
    maxDataValue=max(srcData)+0.2d*abs(max(srcData))
;    print("minDataValue:"+minDataValue)
;    print("maxDataValue:"+maxDataValue)

    ; masking out outliers
    dstData@_FillValue=-999.0
    dstData=where( (dstData.lt.minDataValue).or.(dstData.gt.maxDataValue),  \
                    dstData@_FillValue,dstData)
                    
	; Copying some attributes.
	; Note: The user is responsible for attributes in general.
	if (isatt(srcData,"units")) then
	    print("Copying units attribute ...")
	    dstData@units=srcData@units
	end if
	if (isatt(srcData,"long_name")) then
	    print("Copying long_name attribute ...")
	    dstData@long_name="remapped "+srcData@long_name
	end if
	if (isatt(srcData,"short_name")) then
	    print("Copying short_name attribute ...")
	    dstData@short_name=srcData@short_name
	end if
	print("Don't forget to copy the attributes accordingly from original data!")
	
	return(dstData)
end     ; of esmf_remap(...)

;===================================================================================================
; This function will retrieve the latitude coordinate of a grid
; from a SCRIP formatted file.
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

;===================================================================================================
; This function will retrieve the longitude coordinate of a grid
; from a SCRIP formatted file.
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

;===================================================================================================
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

;===================================================================================================
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

;===================================================================================================
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

;===================================================================================================
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

;===================================================================================================
; Removes those dimensions with one element.
undef("squeeze")
function squeeze(inVar)
begin
    inVardimsizes=dimsizes(inVar)
    newdimsizes=inVardimsizes(ind(inVardimsizes.ne.1))
    outVar=reshape(inVar,newdimsizes)
    
    ;copying the dimension names.
    counter=0;
    do i=0,dimsizes(inVardimsizes)-1
        if (inVardimsizes(i).ne.1) then
            if (isdefined(inVar!i)) then
                tmpName=inVar!i
                outVar!counter=tmpName
                outVar&$tmpName$=inVar&$tmpName$
                counter=counter+1
            end if
        end if
    end do
    
    return(outVar)
end
;===================================================================================================
;===================================================================================================
;===================================================================================================
;===================================================================================================


