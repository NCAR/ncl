/*
 *      $Id$
 */
/************************************************************************
*    								*
*    		     Copyright (C)  2009			*
*     University Corporation for Atmospheric Research		*
*    		     All Rights Reserved			*
*    								*
************************************************************************/
/*
 *    File:		
 *
 *    Author:		
 *    		National Center for Atmospheric Research
 *    		PO 3000, Boulder, Colorado
 *
 *    Date:	    March, 2009
 *
 *    Description:	
 *
 *      The GDAL/OGR library is used to read various file formats as supported by OGR.
 *      OGR is an implementation of the Open-GIS Consortium's "Simple Features" API.
 *
 *      The geometry in an OGR file can be complex; a "feature" might be comprised of one or 
 *      more line segments, which in turn may represent polylines or polygons, etc.
 *      Geometry herein is thus encoded in the following "tables".
 *      
 *      For each "feature" in the OGR file, there is an entry in the "geometry" table.
 *      These entries contain 2 items:
 *      geometry(*, 0) = index into segments table of first segment for the feature
 *      geometry(*, 1) = the number of segments belonging to this feature.
 *
 *      The "segments" table is a partially ordered list of segments (individual points,
 *      polylines, polygons). There are one or more entries in this table per feature; the 
 *      geometry table points to the first segment entry, and all entries belonging to 
 *      the feature directly follow the first. Entries in the segments table provide pointers
 *      to the xy(z) coordinates:
 *      segments(*, 0) = index into X, Y, Z arrays of first point of the segment
 *      segments(*, 1) = number of points in the segment.
 *
 *      The actual geometry coordinates are stored in the "x", "y", and optional "z" arrays.
 *      These contain partially ordered lists of coordinates that make up the individual segments.
 *
 *      Any non-spatial fields associated with a given OGR file are mapped directly to 
 *      an NCL variable associated with the file.
 *
 *      Several global attributes are defined, mainly for convenience in the scripts. One encodes
 *      the "layer name", as extracted from the OGR file. Another denoted whether the set of 
 *      features in the file is comprised of Point, Polylines, or Polygons. The remaining 
 *      attributes are intended as symbolic names into the second index of the geometry and
 *      segments tables.
 *
 *      The OGC/OGR simple features implementation does not expose anything that could/might be
 *      used as variable attributes, so none are created for these file variables. The same is 
 *      true for coordinate variables.
 *
 *      NOTE on 64-bit port: Although the NCL file interface has been modified to accomodate
 *      arrays larger than 2GB, the OGR API has inherent limits of size "int", and this is
 *      reflected internally in this source module.
 *      8/10/2010, RLB.
 *
 */
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclData.h"
#include "NclList.h"
#include "ListSupport.h"
#include "NclAdvancedFile.h"
#include <ogr_api.h>
#include <ogr_srs_api.h>
#include <math.h>
#include <unistd.h>

typedef struct _AdvancedOGRRecord AdvancedOGRRecord;

/* Our Private File Record */
struct _AdvancedOGRRecord
{
  /*
   */
    OGRFeatureH                  feature;
    OGRGeometryH                 geom;
    OGRDataSourceH               dataSource;
    OGRLayerH                    layer;
    OGRFeatureDefnH              layerDefn;
    OGRCoordinateTransformationH xform;

    int is3DGeometry;
};

static int AdvancedOGRInitialized = 0;

/*
 * _is3DGeometry()
 */
static int _is3DGeometry(OGRwkbGeometryType geom)
{
    return (wkbFlatten(geom) != geom);
}


/*
 * _mapOGRType2NCL()
 *
 * Maps types as known by OGR onto types as known by NCL. 
 * NOTE that a number of OGr types have no equivalent in NCL.
 *
 */
static int _mapOGRType2Ncl
#if     NhlNeedProto
(OGRFieldType type)
#else
(type)
OGRFieldType type;
#endif
{
    switch (type) {
        case OFTInteger:  return NCL_int;
        case OFTReal:     return NCL_double;
        case OFTString:   return NCL_string;
        default:      return NCL_none;
    }

    /* imperfect mapping... */
    return NCL_none;
}


/*
 * _mapOGRGeom2NCL()
 *
 * Maps OGR geometry strings onto our internal notion of geometry.
 *
 */
static char* _mapOGRGeom2Ncl
#if     NhlNeedProto
(OGRwkbGeometryType type)
#else
(type)
OGRwkbGeometryType type;
#endif
{
    switch (type) {
        case wkbPoint:  
        case wkbMultiPoint:
        case wkbPoint25D:
        case wkbMultiPoint25D:
            return "point";

        case wkbLineString:
        case wkbMultiLineString:
        case wkbLineString25D:
        case wkbMultiLineString25D:
            return "polyline";
  
        case wkbPolygon:
        case wkbMultiPolygon:
        case wkbPolygon25D:
        case wkbMultiPolygon25D:
            return "polygon";
        default:
    		return "unknown";
    }

    return "unknown";        
}

/*
 * _countGeometry()
 *
 * Utility function used to recursively traverse OGRGeometry to obtain a count
 * of the total number of line-segments and numbers of XY(Z) tuples.
 *
 */
static void _countGeometry(OGRGeometryH geom, ng_size_t *numSegments, ng_size_t *numPoints)
{
    int geomCount = OGR_G_GetGeometryCount(geom);

    if (geomCount == 0) {
        /* presumed to be Point or LineString */
        (*numSegments)++;
        *numPoints += OGR_G_GetPointCount(geom);
    }
    else {
        OGRGeometryH subGeom;
        int i, numPts;  
        for (i=0; i<geomCount; i++) {
            subGeom = OGR_G_GetGeometryRef(geom, i);
            numPts = OGR_G_GetPointCount(subGeom);
            if (numPts == 0)
                _countGeometry(subGeom, numSegments, numPoints);
            else {
                (*numSegments)++;
                *numPoints += numPts;
            }
        }
    }
}

/*
 * _AdvancedLoadFeatureGeometry()
 *
 * Utility function used to recursively load OGRGeometry.
 *
 */
static void _AdvancedLoadFeatureGeometry(AdvancedOGRRecord *rec, OGRGeometryH geom,
                                 NclList vlist, int *numSegments, int *numPoints)
{
    int i;
    char buffer[16];
    void *val = NULL;
    int ndims = 2;
    NclVar var;
    NclQuark  dimnames[2];
    ng_size_t dimsizes[2];

    double *x;
    double *y;
    double *z = NULL;

    int geomCount = OGR_G_GetGeometryCount(geom);
    int numSegPts = OGR_G_GetPointCount(geom);

  /*
   *fprintf(stderr, "\nEnter _AdvancedLoadFeatureGeometry, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgeomCount = %d, numSegPts = %d\n", geomCount, numSegPts);
   */

    if(geomCount == 0)
    {
        if(rec->xform)
        {
            OGR_G_Transform(geom, rec->xform);
        }

        if(rec->is3DGeometry)
        {
            dimsizes[0] = 3;
            sprintf(buffer, "xyz_%6.6d", *numSegments);
        }
        else
        {
            dimsizes[0] = 2;
            sprintf(buffer, "xy_%6.6d", *numSegments);
        }

        dimnames[0] = NrmStringToQuark(buffer);

      /*
       *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\t%s, points = %d\n", buffer, *numSegments);
       */

        sprintf(buffer, "pts_%6.6d", *numSegments);
        dimsizes[1] = numSegPts;
        dimnames[1] = NrmStringToQuark(buffer);

        val = (void *)NclCalloc(dimsizes[0] * dimsizes[1], sizeof(double));

        x = (double *) val;
        y = (double *) (val + numSegPts * sizeof(double));
        if(rec->is3DGeometry)
            z = (double *) (val + 2 * numSegPts * sizeof(double));

        for(i = 0; i < numSegPts; i++)
        {
            x[i] = OGR_G_GetX(geom, i);
            y[i] = OGR_G_GetY(geom, i);
            if(rec->is3DGeometry)
                z[i] = OGR_G_GetZ(geom, i);
            (*numPoints)++;
        }

        sprintf(buffer, "seg_%6.6d", *numSegments);
        var = _NclCreateVlenVar(buffer, val, ndims, dimnames, dimsizes, NCL_double);
        _NclListAppend((NclObj)vlist, (NclObj)var);

        (*numSegments)++;
    }
    else
    {
      /*compound geometry*/
        OGRGeometryH subGeom;
        for(i = 0; i < geomCount; i++)
        {
            subGeom = OGR_G_GetGeometryRef(geom, i);
            _AdvancedLoadFeatureGeometry(rec, subGeom, vlist, numSegments, numPoints);
        }
    }

  /*
   *fprintf(stderr, "Leave _AdvancedLoadFeatureGeometry, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
}


/*
 * _AdvancedLoadGeometry()
 *
 * Intended to be called once to load and cache all of the geometry variables, whenever
 * any one of them is asked for.
 *
 */
static int _AdvancedLoadGeometry(NclFileGrpNode *grpnode, NclList vlist)
{
    AdvancedOGRRecord *rec = (AdvancedOGRRecord *) grpnode->other_src;
    OGRFeatureH feature;
    OGRGeometryH geom;
    int segmentNum = 0;
    int pointNum   = 0;
    int geomRecord   = 0;

    char buffer[16];

    NclList glist = NULL;
    NclMultiDValData g_md;
    ng_size_t one = 1;
    int *id = NULL;

  /*
   *fprintf(stderr, "\nEnter _AdvancedLoadGeometry, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    OGR_L_ResetReading(rec->layer);
    feature = OGR_L_GetNextFeature(rec->layer);

    while(NULL != feature)
    {
        id = (int *)NclMalloc(sizeof(int));
        sprintf(buffer, "geo_%6.6d", geomRecord);
        glist = (NclList)_NclListCreate(NULL, NULL, 0, 0, NCL_FIFO);
        assert(glist);
        _NclListSetType((NclObj)glist,NCL_FIFO);
        glist->list.list_quark = NrmStringToQuark(buffer);
        glist->list.list_type = NrmStringToQuark("FIFO");
        glist->obj.obj_type = Ncl_List;
        *id = glist->obj.id;
        g_md = _NclMultiDVallistDataCreate(NULL,NULL,Ncl_MultiDVallistData,0,id,
                                           NULL,1,&one,TEMPORARY,NULL);

        _NclListAppend((NclObj)vlist, (NclObj)g_md);

        geom = OGR_F_GetGeometryRef(feature);
        _AdvancedLoadFeatureGeometry(rec, geom, glist, &segmentNum, &pointNum);
        OGR_F_Destroy(feature);
        feature = OGR_L_GetNextFeature(rec->layer);
	++geomRecord;
	segmentNum = 0;
	pointNum = 0;
    }
 
  /*
   *fprintf(stderr, "Leave _AdvancedLoadGeometry, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return 1;
}

/*
 * When it comes to extracting field variables, we set a pointer to a function of
 * this type that can extract and return a field of the proper NCL-type.
 */
typedef void (*FieldExtractor)(OGRFeatureH feature, int fieldNum, void* storage, long offset);


/*
 * _getFieldAsInteger()
 *
 * A FieldExtractor for integer fields.
 */
static void _getFieldAsInteger(OGRFeatureH feature, int fieldNum, void* storage, long offset)
{
    int fieldVal = OGR_F_GetFieldAsInteger(feature, fieldNum);
    *( (int*)(storage)+offset) = fieldVal;
}


/*
 * _getFieldAsDouble
 *
 * A FieldExtractor for real-valued fields.
 */
static void _getFieldAsDouble(OGRFeatureH feature, int fieldNum, void* storage, long offset)
{
    double fieldVal = OGR_F_GetFieldAsDouble(feature, fieldNum);
    *( (double*)(storage)+offset) = fieldVal;
}


/*
 * _getFieldAsString
 *
 * A FieldExtractor for string fields.
 */
static void _getFieldAsString(OGRFeatureH feature, int fieldNum, void* storage, long offset)
{
    NclQuark quark = NrmStringToQuark(OGR_F_GetFieldAsString(feature, fieldNum));
    *( (NclQuark*)(storage)+offset) = quark;
}

/*
 * AdvancedOGRInitializeFileRec()
 */
static void *AdvancedOGRInitializeFileRec(NclFileFormat *format)
{
    NclFileGrpNode *grpnode = NULL;

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tAdvancedOGRInitializeFileRec...\n");
   */

    *format = _NclAdvancedOGR;

    grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
    assert(grpnode);

    grpnode->fid = -1;
    grpnode->gid = -1;
    grpnode->pid = -1;
    grpnode->name = NrmStringToQuark("/");
    grpnode->pname = -1;
    grpnode->real_name = NrmStringToQuark("/");
    grpnode->path = -1;
    grpnode->extension = -1;

    grpnode->open = 0;
    grpnode->header_reserve_space = 0;
    grpnode->define_mode = 0;
    grpnode->other_src = NULL;
    grpnode->parent = NULL;

    return (void *) grpnode;
}

/*
 * _setGroupDims()
 *
 * Creates dimension records for our fixed set of dimensions.
 *
 */
static void _setGroupDims(NclFileGrpNode *grpnode,
                          int numGeometry,
                          int numSegments,
                          int numPoints)
{
    /* the per-feature dimension  (fill in size later) */
    _addNclDimNode(&(grpnode->dim_rec), NrmStringToQuark("features"),
                   0, (ng_size_t)numGeometry, 1);

    /* the segments (fill in size later) */
    _addNclDimNode(&(grpnode->dim_rec), NrmStringToQuark("segments"),
                   1, (ng_size_t)numSegments, 0);

    /* the XY(Z) points (fill in size later) */
    _addNclDimNode(&(grpnode->dim_rec), NrmStringToQuark("points"),
                   2, (ng_size_t)numPoints, 0);
}

/*
 * _setGroupAtts()
 *
 * Defines attribute records for our fixed set of global attributes.
 *
 */
static void _setGroupAtts(NclFileGrpNode *grpnode, OGRFeatureDefnH layerDefn,
                          int numGeometry,
                          int numSegments,
                          int numPoints,
                          int *geometryRecord)
{
    int iv = 0;
    NclQuark qname = -1;
    int *ipv = (int *)NclMalloc(numGeometry * sizeof(int));

    /* the layer name */
    qname =  NrmStringToQuark(OGR_FD_GetName(layerDefn));
    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("layer_name"),
                   NCL_string, 1, (void *)&qname);

    /* the geometry-type of the layer */
    qname = NrmStringToQuark(_mapOGRGeom2Ncl(OGR_FD_GetGeomType(layerDefn)));
    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("geometry_type"),
                   NCL_string, 1, (void *)&qname);

    /* The remaining global-atts are "convenience constants" intended to be used
     * in scripting code to index into the geometry and segments variables.
     */

    iv = numGeometry;
    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("numGeom"),
                   NCL_int, 1, (void *) &iv);

    iv = numSegments;
    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("numSegs"),
                   NCL_int, 1, (void *) &iv);

    iv = numPoints;
    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("numPnts"),
                   NCL_int, 1, (void *) &iv);

    memcpy(ipv, geometryRecord, numGeometry*sizeof(int));
    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("geoRecord"),
                   NCL_int, numGeometry, (void *) ipv);
}

/*
 * _setGroupVars()
 * Defines variables records for this OGR file. There is a predetermined set of variables,
 * referred to collectively as the "geometry variables" ("geometry", "segments",
 * "x", "y", and optionally "z"). Then we create variables for each (non-spatial) field
 * associated with records in the OGR file.
 *
 */
static void _setGroupVars(NclFileGrpNode *grpnode,
                          OGRFeatureDefnH layerDefn)
{
    OGRFieldDefnH fldDef;
    int numVars;
    int i = 0;
    int j = 0;

    NclQuark dim_names[NCL_MAX_DIMENSIONS];
    long dim_sizes[NCL_MAX_DIMENSIONS];
    NclFileDimNode *dimnode = grpnode->dim_rec->dim_node;

    /* We have a fixed set of variables that are common to all OGR datasets,
     * plus a set that are specific to the given dataset.
     */

    numVars = OGR_FD_GetFieldCount(layerDefn);
  /*
   *March through the layer definition to get the dataset-specific variable defns.
   *Note that these variables exist 1:1 with features, and are thus same length as
   *the geometry table.
   */
    for(j = 0; j < numVars; j++)
    {
        fldDef = OGR_FD_GetFieldDefn(layerDefn, j);
        dim_names[0] = dimnode[0].name;
        dim_sizes[0] = dimnode[0].size;
        _addNclVarNodeToGrpNode(grpnode, NrmStringToQuark(OGR_Fld_GetNameRef(fldDef)),
                                j, _mapOGRType2Ncl(OGR_Fld_GetType(fldDef)),
                                1, dim_names, dim_sizes);
        ++i;
    }

   /*the "segments" encodes the segments (x, y[, z])*/
    dim_names[0] = NrmStringToQuark("segment");
    dim_sizes[0] = 1;
    _addNclVarNodeToGrpNode(grpnode, NrmStringToQuark("segments"),
                            i, NCL_list, 1, dim_names, dim_sizes);
}

/*
 * AdvancedOGROpenFile()
 */
static void *AdvancedOGROpenFile(void *therec, NclQuark path, int wr_status)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;

    AdvancedOGRRecord *rec = NULL;
    OGRLayerH layer;
    OGRFeatureH feature;
    OGRGeometryH geom;
    OGRSpatialReferenceH sSrs;
    OGRwkbGeometryType geomType;

    int numGeometry = 0;
    int numSegments = 0;
    int numPoints = 0;
    int segmentsCounter = 0;
    int geometryCounter = 1024;
    int *geometryRecord = (int *)NclCalloc(geometryCounter, sizeof(int));
    if(NULL == geometryRecord)
    {
	NhlPError(NhlFATAL, NhlEUNKNOWN,
			    "Unable to allocate memeory for geometryRecord\n");
    	return grpnode;
    }

    rec = (AdvancedOGRRecord*)NclCalloc(1, sizeof(AdvancedOGRRecord));
    if(NULL == rec)
    	return grpnode;

    if(!AdvancedOGRInitialized)
    {
      /*Initialize the OGR drivers, and make sure OGR is one of them...*/
        OGRRegisterAll();
        AdvancedOGRInitialized = 1;
    }

    /* NOTE: for now we only support read-only access */
    if(0 == wr_status)
    {
        NhlPError(NhlWARNING, NhlEUNKNOWN,
            "writing to OGR files is not currently implemented.");
        wr_status = 1;
    }

    rec->dataSource = OGROpen(NrmQuarkToString(path), !wr_status, NULL);
    if(NULL == rec->dataSource)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Failed to open OGR file: ");
        NhlPError(NhlFATAL, NhlEUNKNOWN, NrmQuarkToString(path));
        return grpnode;
    }       

    grpnode->path = path;
    grpnode->status = wr_status;
    grpnode->other_src = (void *)rec;

  /*extract first feature of *FIRST* layer, which will be the basis for this 
   *file definition.
   */
    layer = OGR_DS_GetLayer(rec->dataSource, 0);
    if(NULL == layer)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "No layers found in OGR file:");
        NhlPError(NhlFATAL, NhlEUNKNOWN, NrmQuarkToString(path));
        return grpnode;
    }

    rec->layerDefn = OGR_L_GetLayerDefn(layer);
    rec->layer = layer;
    
    geomType = OGR_FD_GetGeomType(rec->layerDefn);
    if(_is3DGeometry(geomType))
        rec->is3DGeometry = 1;
    else
        rec->is3DGeometry = 0;

    rec->xform = NULL;
    sSrs = OGR_L_GetSpatialRef(rec->layer);
    if((NULL == sSrs) && OSRIsGeographic(sSrs))
    {
        return grpnode;
    }

    OGRSpatialReferenceH tSrs = OSRNewSpatialReference(NULL); 
    OSRSetWellKnownGeogCS(tSrs, "WGS84");  /* LonLat */
    rec->xform = OCTNewCoordinateTransformation(sSrs, tSrs); 

  /*Read through the geometry to get the number of features, segments, and points...*/
    OGR_L_ResetReading(layer);
    feature = OGR_L_GetNextFeature(layer);
    geom = OGR_F_GetGeometryRef(feature);

    while(NULL != feature)
    {
        geom = OGR_F_GetGeometryRef(feature);
        segmentsCounter = 0;
        _countGeometry(geom, &segmentsCounter, &numPoints);
      /*OGR_F_Destroy(feature);*/
        geometryRecord[numGeometry] = segmentsCounter;

        ++numGeometry;
        if(numGeometry > geometryCounter)
	{
            geometryCounter *= 2;
            geometryRecord = (int *)NclRealloc(geometryRecord, geometryCounter*sizeof(ng_size_t));
            if(NULL == geometryRecord)
            {
	        NhlPError(NhlFATAL, NhlEUNKNOWN,
			           "Unable to reallocate memeory for geometryRecord\n");
    	       return grpnode;
            }
        }
        feature = OGR_L_GetNextFeature(layer);
      /*numSegments += segmentsCounter;*/
    }

    _setGroupDims(grpnode, numGeometry, numSegments, numPoints);
    _setGroupAtts(grpnode, rec->layerDefn, numGeometry, numSegments, numPoints, geometryRecord);
    _setGroupVars(grpnode, rec->layerDefn);

    NclFree(geometryRecord);

    return((void*)grpnode);
}

/*
 * AdvancedOGRCreateFile()
 *
 */
static void *AdvancedOGRCreateFile(void *rec,NclQuark path)
{
    return AdvancedOGROpenFile(rec,path,1);
}

/*
 * _getFieldVariable()
 *
 * Returns the contents of the requested (non-spatial) field.
 *
 */
static void *_getFieldVariable(NclFileGrpNode *grpnode, NclQuark thevar,
                               long *start, long *finish,
                               long *stride, void *storage)
{
    AdvancedOGRRecord *rec = (AdvancedOGRRecord *) grpnode->other_src;
    NclFileVarNode *varnode;

    FieldExtractor helper;
    long i, offset;

  /*
   *fprintf(stderr, "\nHit AdvancedOGRReadVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   */

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);

    if(NULL != varnode)
    {
        /* Based upon the type of the variable we're after, we'll invoke a different 
         * helper function in the loop below.  Note that the caller has already 
         * verified varNum as a valid index.
         */
        switch(varnode->type)
        {
            case NCL_int:
                 helper = &_getFieldAsInteger;
                 break;
            case NCL_double:
                 helper = &_getFieldAsDouble;
                 break;
            case NCL_string:
                 helper = &_getFieldAsString;
                 break;
            default:
                 return NULL;
        }
        
        offset = 0;
        OGR_L_ResetReading(rec->layer);

        /* NOTE that OGR fields are always 1-dimensional */
        for(i=start[0]; i<= finish[0]; i+=stride[0])
        {
            OGRFeatureH feature = OGR_L_GetFeature(rec->layer, i);
                
            /* get the field corresponding to varNum. */
            (*helper)(feature, varnode->id, storage, offset++);
              
            OGR_F_Destroy(feature);
        }

        return storage;
    }

    return NULL;
}

/*
 * _getGeometryVariable()
 * 
 * Utility to return the contents of one of the geometry variables.
 *
 */
static void *_getGeometryVariable(NclFileGrpNode *grpnode, NclQuark thevar,
                                  long *start, long *finish,
                                  long *stride, void *storage)
{
  /*
   *fprintf(stderr, "\nEnter _getGeometryVariable, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   *fprintf(stderr, "\t start[0] = %ld\n",  start[0]);
   *fprintf(stderr, "\tfinish[0] = %ld\n", finish[0]);
   *fprintf(stderr, "\tstride[0] = %ld\n", stride[0]);
   */

    if(NrmStringToQuark("segments") == thevar)
    {
        NclList vlist = NULL;
        NclMultiDValData v_md;
        ng_size_t one = 1;
        int *id = (int *)NclMalloc(sizeof(int));

        vlist = (NclList)_NclListCreate(NULL, NULL, 0, 0, NCL_FIFO);
        assert(vlist);
        _NclListSetType((NclObj)vlist,NCL_FIFO);
        vlist->list.list_quark = NrmStringToQuark("segments_list");
        vlist->list.list_type = NrmStringToQuark("FIFO");
        vlist->obj.obj_type = Ncl_List;
        *id = vlist->obj.id;
        v_md = _NclMultiDVallistDataCreate(NULL,NULL,Ncl_MultiDVallistData,0,id,
                                           NULL,1,&one,TEMPORARY,NULL);

        _AdvancedLoadGeometry(grpnode, vlist);

      /*
       *fprintf(stderr, "Leave _getGeometryVariable, file: %s, line: %d\n\n", __FILE__, __LINE__);
       */

        return (void *)v_md;
    }
    else
        return NULL;

    return storage;
}

/*
 * AdvancedOGRReadVar()
 *
 */
static void *AdvancedOGRReadVar(void* therec, NclQuark thevar,
                               long* start, long* finish,
                               long* stride, void* storage)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    NclFileVarNode *varnode;

  /*
   *fprintf(stderr, "\nfile %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvar name: <%s>\n", NrmQuarkToString(thevar));
   */

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);

    if(NULL != varnode)
    {
        /* found the requested variable... */
        if(NrmStringToQuark("segments") == thevar)
            return _getGeometryVariable(grpnode, thevar, start, finish, stride, storage);
        else
            return _getFieldVariable(grpnode, thevar, start, finish, stride, storage);
    }

    return NULL;
}


/* 
 * AdvancedOGRReadCoord()
 *
 */
static void *AdvancedOGRReadCoord(void* therec, NclQuark thevar,
                                 long* start, long* finish,
                                 long* stride, void* storage)
{
    return(AdvancedOGRReadVar(therec,thevar,start,finish,stride,storage));
}

NclFormatFunctionRec AdvancedOGRRec =
{
/* NclInitializeFileRecFunc initialize_file_rec */      AdvancedOGRInitializeFileRec,
/* NclCreateFileFunc       create_file; */		AdvancedOGRCreateFile,
/* NclOpenFileFunc     open_file; */    	        AdvancedOGROpenFile,
/* NclFreeFileRecFunc      free_file_rec; */    	NULL,
/* NclGetVarNamesFunc      get_var_names; */    	NULL,
/* NclGetVarInfoFunc       get_var_info; */    	        NULL,
/* NclGetDimNamesFunc      get_dim_names; */    	NULL,
/* NclGetDimInfoFunc       get_dim_info; */    	        NULL,
/* NclGetAttNamesFunc      get_att_names; */    	NULL,
/* NclGetAttInfoFunc       get_att_info; */    	        NULL,
/* NclGetVarAttNamesFunc   get_var_att_names; */        NULL,
/* NclGetVarAttInfoFunc    get_var_att_info; */    	NULL,
/* NclGetCoordInfoFunc     get_coord_info; */    	NULL,
/* NclReadCoordFunc    read_coord; */    	        AdvancedOGRReadCoord,
/* NclReadCoordFunc    read_coord; */    	        NULL,
/* NclReadVarFunc      read_var; */    		        AdvancedOGRReadVar,
/* NclReadVarFunc      read_var; */    		        NULL,
/* NclReadAttFunc      read_att; */    		        NULL,
/* NclReadVarAttFunc       read_var_att; */    	        NULL,
/* NclWriteCoordFunc       write_coord; */    	        NULL,
/* NclWriteCoordFunc       write_coord; */    	        NULL,
/* NclWriteVarFunc     write_var; */    	        NULL,
/* NclWriteVarFunc     write_var; */    	        NULL,
/* NclWriteAttFunc     write_att; */    	        NULL,
/* NclWriteVarAttFunc      write_var_att; */    	NULL,
/* NclAddDimFunc       add_dim; */    		        NULL,
/* NclAddChunkDimFunc      add_chunk_dim; */    	NULL,
/* NclRenameDimFunc    rename_dim; */    	        NULL,
/* NclAddVarFunc       add_var; */    		        NULL,
/* NclAddVarChunkFunc      add_var_chunk; */    	NULL,
/* NclAddVarChunkCacheFunc add_var_chunk_cache; */      NULL,
/* NclSetVarCompressLevelFunc set_var_compress_level; */ NULL,
/* NclAddVarFunc       add_coord_var; */    	        NULL,
/* NclAddAttFunc       add_att; */    		        NULL,
/* NclAddVarAttFunc    add_var_att; */    	        NULL,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */   NULL,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */   NULL,
/* NclDelAttFunc       del_att; */    		        NULL,
/* NclDelVarAttFunc    del_var_att; */    	        NULL,
/* NclGetGrpNamesFunc      get_grp_names; */            _NclGetGrpNames,
/* NclGetGrpInfoFunc       get_grp_info; */             NULL,
/* NclGetGrpAttNamesFunc   get_grp_att_names; */        NULL,
/* NclGetGrpAttInfoFunc    get_grp_att_info; */         NULL,
/* NclAddGrpFunc           add_grp; */                  NULL,
/* NclAddVlenFunc          add_vlen; */                 NULL,
/* NclAddEnumFunc          add_enum; */                 NULL,
/* NclAddOpaqueFunc        add_opaque; */               NULL,
/* NclAddCompoundFunc      add_compound; */             NULL,
/* NclWriteCompoundFunc    write_compound; */           NULL,
/* NclSetOptionFunc    set_option;  */                  NULL
};


/*
 * AdvancedOGRAddFileFormat()
 */
NclFormatFunctionRecPtr AdvancedOGRAddFileFormat(void)
{    
    return(&AdvancedOGRRec);
}

