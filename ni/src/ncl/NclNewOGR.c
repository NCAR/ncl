/*
 *      $Id: NclOGR.c,v 1.5 2010-05-06 22:52:28 huangwei Exp $
 */
/************************************************************************
*    								*
*    		     Copyright (C)  2009			*
*         University Corporation for Atmospheric Research		*
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
 *    Date:	        March, 2009
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
#include "NclVar.h"
#include "NclNewFile.h"
#include "ListSupport.h"

#include <ogr_api.h>
#include <ogr_srs_api.h>
#include <math.h>
#include <unistd.h>
#include <assert.h>

/* #define OGR_DEBUG 1 */

typedef struct _OGRRecord OGRRecord;

/* Our Private File Record */
struct _OGRRecord
{
    OGRFeatureH      feature;
    OGRGeometryH     geom;
    OGRDataSourceH   dataSource;
    OGRLayerH        layer;
    OGRFeatureDefnH  layerDefn;
    OGRCoordinateTransformationH xform;

    int              is3DGeometry;
};

static int NewOGRInitialized = 0;

extern void _NclBuildArrayOfList(void *tmp_val, int ndims, ng_size_t *dim_sizes);

/*
 * _is3DGeometry()
 *
 */
static int _is3DGeometry(OGRwkbGeometryType geom)
{
        return (wkbFlatten(geom) != geom);
}

#if 0
static NclNewList _CreateVlist4OGR(NclQuark name)
{
    NclNewList vlist = NULL;

    vlist = (NclNewList)_NclNewListCreate(NULL, NULL, 0, 0, -1, (NCL_ITEM | NCL_FIFO));
    assert(vlist);
    _NclListSetType((NclObj)vlist, NCL_ITEM);
    vlist->newlist.name = name;
    vlist->newlist.type = NrmStringToQuark("item");
    vlist->obj.obj_type = Ncl_List;

    return vlist;
}
#endif

/*
 * _mapOGRType2NCL()
 *
 * Maps types as known by OGR onto types as known by NCL. 
 * NOTE that a number of OGr types have no equivalent in NCL.
 *
 */
static NclBasicDataTypes _mapOGRType2Ncl(OGRFieldType type)
{
    switch (type)
    {
        case OFTInteger:
             return NCL_int;
        case OFTReal:
             return NCL_double;
        case OFTString:
             return NCL_string;
        default:
             return NCL_none;
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
static char* _mapOGRGeom2Ncl(OGRwkbGeometryType type)
{
    switch (type)
    {
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
                          int numPoints)
{
    NclQuark qname = -1;

  /*the layer name*/
    qname = NrmStringToQuark(OGR_FD_GetName(layerDefn));
    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("layer_name"),
                   NCL_string, 1, (void *)&qname);

    /* the geometry-type of the layer */
    qname = NrmStringToQuark(_mapOGRGeom2Ncl(OGR_FD_GetGeomType(layerDefn)));
    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("geometry_type"),
                   NCL_string, 1, (void *)&qname);

  /*
   *The remaining global-atts are "convenience constants" intended to be used 
   *in scripting code to index into the geometry and segments variables.
   */

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tGeometry %d: segs: %d, points: %d\n",
   *                 numGeometry, numSegments, numPoints);
   */
    
    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("numGeom"),
                   NCL_int, 1, (void *)&numGeometry);

    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("numSegs"),
                   NCL_int, 1, (void *)&numSegments);

    _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark("numPnts"),
                   NCL_int, 1, (void *)&numPoints);
}

/*
 * _buildArrayOfListVar()
 *
 * Defines a variable as an array of list.
 *
 */
static NclMultiDValData _buildArrayOfListVar(int numGeometry)
{
    NclObjTypes the_obj_type;
    NclBasicDataTypes the_type;
    NclMultiDValData tmp_md;
    void *tmp_val;
    ng_size_t ndims = 1;
    ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
    ng_usize_t totalsize = 1;
    
    the_type = NCL_list;
    the_obj_type = Ncl_Typelist;

    dim_sizes[0] = numGeometry;

    totalsize = numGeometry * _NclSizeOf(the_type);

    tmp_val = (void*)NclMalloc(totalsize);
    if(! tmp_val)
    {
        NhlPError(NhlFATAL,ENOMEM,"New: could not create new array");
        return NULL;
    }

    _NclBuildArrayOfList(tmp_val, ndims, dim_sizes);

    tmp_md = _NclCreateVal(NULL, NULL, Ncl_MultiDValData, 0, tmp_val,
                           NULL, ndims, dim_sizes, TEMPORARY, NULL,
                           _NclTypeEnumToTypeClass(the_obj_type));

    return tmp_md;
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
                          OGRFeatureDefnH layerDefn,
                          int numGeometry,
                          int numSegments,
                          int numPoints)
{
    OGRFieldDefnH fldDef;
    int numVars;
    int j = 0;
    int ndims = 1;

    NclQuark dim_names[NCL_MAX_DIMENSIONS];
    long dim_sizes[NCL_MAX_DIMENSIONS];
    NclFileDimNode *dimnode = grpnode->dim_rec->dim_node;

    /* We have a fixed set of variables that are common to all OGR datasets, 
     * plus a set that are specific to the given dataset.
     */

    numVars = OGR_FD_GetFieldCount(layerDefn);
    /* March through the layer definition to get the dataset-specific variable defns.
     * Note that these variables exist 1:1 with features, and are thus same length as 
     * the geometry table.
     */
    for(j = 0; j < numVars; j++)
    {
        fldDef = OGR_FD_GetFieldDefn(layerDefn, j);
        dim_names[0] = dimnode[0].name;
        dim_sizes[0] = dimnode[0].size;
        _addNclVarNodeToGrpNode(grpnode, NrmStringToQuark(OGR_Fld_GetNameRef(fldDef)),
                                j+1, _mapOGRType2Ncl(OGR_Fld_GetType(fldDef)),
                                1, dim_names, dim_sizes);
    }

    dim_names[0] = dimnode[0].name;
    dim_sizes[0] = numGeometry;

    _addNclVarNodeToGrpNode(grpnode, NrmStringToQuark("Geometry"),
                            numVars, NCL_list,
                            ndims, dim_names, dim_sizes);
}

/*
 * _countSubGeometry()
 *
 * Utility function used to recursively traverse OGRGeometry to obtain a count
 * of the total number of line-segments and numbers of XY(Z) tuples.
 *
 */
static void _countSubGeometry(OGRGeometryH geom,
                              int *numSegments,
                              int *numPoints)
{
    int i, numPts;

    int geomCount = OGR_G_GetGeometryCount(geom);

  /*
   *fprintf(stderr, "\nEnter _countSubGeometry, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrpnode->name: <%s>\n", NrmQuarkToString(grpnode->name));
   */

    if (geomCount == 0)
    {
        /* presumed to be Point or LineString */
        numPts = OGR_G_GetPointCount(geom);
        *numPoints += numPts;

        (*numSegments)++;
    }
    else
    {
        OGRGeometryH subGeom;
        for(i = 0; i < geomCount; i++)
        {
            subGeom = OGR_G_GetGeometryRef(geom, i);
            numPts = OGR_G_GetPointCount(subGeom);
            if(numPts == 0)
            {
                int locSegments = 0;
                int locPoints = 0;
                _countSubGeometry(subGeom, &locSegments, &locPoints);
                (*numSegments) += locSegments;
                *numPoints += locPoints;
            }
            else
            {
                (*numSegments)++;
                *numPoints += numPts;
            }
        }
    }

  /*
   *fprintf(stderr, "\tgeomCount = %d, numSegments = %d, numPoints = %d\n",
   *                   geomCount, *numSegments, *numPoints);
   *fprintf(stderr, "Leave _countSubGeometry, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
}

/*
 * _countGeometry()
 *
 * Utility function used to recursively traverse OGRGeometry to obtain a count
 * of the total number of line-segments and numbers of XY(Z) tuples.
 *
 */
static void _countGeometry(OGRGeometryH geom,
                           int *numSegments,
                           int *numPoints)
{
    int i, numPts;

    int geomCount = OGR_G_GetGeometryCount(geom);

    if (geomCount == 0)
    {
        /* presumed to be Point or LineString */
        numPts = OGR_G_GetPointCount(geom);
        *numPoints += numPts;

        (*numSegments)++;
    }
    else
    {
        OGRGeometryH subGeom;
        for(i = 0; i < geomCount; i++)
        {
            subGeom = OGR_G_GetGeometryRef(geom, i);
            numPts = OGR_G_GetPointCount(subGeom);
            if(numPts == 0)
            {
                int locSegments = 0;
                int locPoints = 0;
                _countSubGeometry(subGeom, &locSegments, &locPoints);
                (*numSegments) += locSegments;
                *numPoints += locPoints;
            }
            else
            {
                (*numSegments)++;
                *numPoints += numPts;
            }
        }
    }
}

/*
 * _loadSubGeometry()
 *
 * Utility function used to recursively load OGRGeometry.
 *
 */
static void _loadSubGeometry(OGRRecord *rec, OGRGeometryH geom,
                            NclList sublist, int numSegments)
{
    char buffer[16];
    void *val = NULL;

    int i;
    int ndims = 2;
    NclVar var;
    NclQuark  dimnames[2];
    ng_size_t dimsizes[2];

    double *x;
    double *y;
    double *z = NULL;

    int numSegPts = OGR_G_GetPointCount(geom);

  /*
   *fprintf(stderr, "\tEnter %s, file: %s, line: %d\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);

   *fprintf(stderr, "\t\tfunction %s, file: %s, line: %d\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);
   *fprintf(stderr, "\t\t\tnumSegPts = %d\n", numSegPts);
   */

    if(rec->xform)
        OGR_G_Transform(geom, rec->xform);

    sprintf(buffer, "xyz_%6.6d", numSegments);
    if(rec->is3DGeometry)
        dimsizes[0] = 3;
    else
        dimsizes[0] = 2;
    dimnames[0] = NrmStringToQuark(buffer);

    sprintf(buffer, "pts_%6.6d", numSegments);
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
    }

    sprintf(buffer, "seg_%6.6d", numSegments);
    var = _NclCreateVlenVar(buffer, val, ndims, dimnames, dimsizes, NCL_double);
    _NclListPush((NclObj)sublist, (NclObj)var);

  /*
    _NclListAppend((NclObj)sublist, (NclObj)var);
   *fprintf(stderr, "\tLeave %s, file: %s, line: %d\n\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);
   */
}

/*
 * _loopSubGeometry()
 *
 * Utility function used to recursively load OGRGeometry.
 *
 */
static void _loopSubGeometry(OGRRecord *rec, OGRGeometryH geom,
                            NclList sublist, int numSegments)
{
    int i;
    int geomCount = OGR_G_GetGeometryCount(geom);

  /*
   *fprintf(stderr, "\tEnter %s, file: %s, line: %d\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);
   */

    if(0 == geomCount)
    {
        _loadSubGeometry(rec, geom, sublist, numSegments);
    }
    else
    {
      /*compound geometry*/
        OGRGeometryH subGeom;

      /*
       *fprintf(stderr, "\tfunction %s, file: %s, line: %d\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);
       *fprintf(stderr, "\t\tgeomCount = %d\n", geomCount);
       */

        for(i = 0; i < geomCount; i++)
        {
            subGeom = OGR_G_GetGeometryRef(geom, i);
            _loopSubGeometry(rec, subGeom, sublist, numSegments + i);
        }
    }

  /*
   *fprintf(stderr, "\tLeave %s, file: %s, line: %d\n\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);
   */
}

/*
 * _loadFeatureGeometry()
 *
 * Utility function used to recursively load OGRGeometry.
 *
 */
static void *_loadFeatureGeometry(OGRRecord *rec, NclFileVarNode *varnode,
                                long *start, long *finish, long *stride)
{
    OGRFeatureH feature;
    OGRGeometryH geom;
    OGRGeometryH subGeom;

    int numGeometrys = 0;
    int i, n;
    int geomCount = 0;
    long idx;

    long local_start, local_finish, local_stride;

    NclMultiDValData geo_tmp_md = NULL;
    int *geo_obj_id = NULL;
    NclList geo_list;

  /*
   *fprintf(stderr, "\nEnter _loadFeatureGeometry, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    if(NULL != varnode->dim_rec)
        numGeometrys = (int) varnode->dim_rec->dim_node[0].size;
    else
        return NULL;

    numGeometrys = (finish[0] - start[0] + 1) / stride[0];
    if(0 > numGeometrys)
    {
        numGeometrys *= (-1);
        local_start = finish[0];
        local_finish = start[0];
        local_stride = -stride[0];
    }
    else
    {
        local_start  = start[0];
        local_finish = finish[0];
        local_stride = stride[0];
    }

    if(0 == numGeometrys)
        return NULL;

    geo_tmp_md = _buildArrayOfListVar(numGeometrys);
    geo_obj_id = (int *)geo_tmp_md->multidval.val;
    n = 0;

    for(idx = local_start; idx <= local_finish; idx += local_stride)
    {
        feature = OGR_L_GetNextFeature(rec->layer);
        if(NULL == feature)
            continue;

        geo_list = (NclList)_NclGetObj(geo_obj_id[n]);

        geom = OGR_F_GetGeometryRef(feature);

        geomCount = OGR_G_GetGeometryCount(geom);

        if(0 == geomCount)
        {
            fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
            fprintf(stderr, "\tGeometry %d: geomCount = %d\n", n, geomCount);
            continue;
        }

      /*
       *fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tGeometry %d: geomCount = %d\n", n, geomCount);
       */

        for(i = 0; i < geomCount; ++i)
        {
            subGeom = OGR_G_GetGeometryRef(geom, i);
            _loopSubGeometry(rec, subGeom, geo_list, i);
        }

        ++n;
    }

  /*
   *fprintf(stderr, "Leave _loadFeatureGeometry, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return (void *)geo_tmp_md;
}
  

/*
 * When it comes to extracting field variables, we set a pointer to a function of
 * this type that can extract and return a field of the proper NCL-type.
 */
typedef void (*FieldExtractor)(
#if    NhlNeedProto
OGRFeatureH feature, int fieldNum, void* storage, long offset
#endif
);

/*
 * _getFieldAsInteger()
 *
 * A FieldExtractor for integer fields.
 */
static void _getFieldAsInteger
#if    NhlNeedProto
(OGRFeatureH feature, int fieldNum, void* storage, long offset)
#else
(feature, fieldNum, storage, offset)
OGRFeatureH feature;
int         fieldNum;
void        *storage;
long        offset;
#endif
{
        int fieldVal = OGR_F_GetFieldAsInteger(feature, fieldNum);
        *( (int*)(storage)+offset) = fieldVal;
}


/*
 * _getFieldAsDouble
 *
 * A FieldExtractor for real-valued fields.
 */
static void _getFieldAsDouble
#if    NhlNeedProto
(OGRFeatureH feature, int fieldNum, void* storage, long offset)
#else
(feature, fieldNum, storage, offset)
OGRFeatureH feature;
int         fieldNum;
void        *storage;
long        offset;
#endif
{
        double fieldVal = OGR_F_GetFieldAsDouble(feature, fieldNum);
        *( (double*)(storage)+offset) = fieldVal;
}


/*
 * _getFieldAsString
 *
 * A FieldExtractor for string fields.
 */
static void _getFieldAsString
#if    NhlNeedProto
(OGRFeatureH feature, int fieldNum, void* storage, long offset)
#else
(feature, fieldNum, storage, offset)
OGRFeatureH feature;
int         fieldNum;
void        *storage;
long        offset;
#endif
{
        NclQuark quark = NrmStringToQuark(OGR_F_GetFieldAsString(feature, fieldNum));
        *( (NclQuark*)(storage)+offset) = quark;
}

/*
 * _getFieldVariable()
 *
 * Returns the contents of the requested (non-spatial) field.
 *
 */
static void *_getFieldVariable(NclFileGrpNode *grpnode, NclFileVarNode *varnode,
                               long *start, long *finish,
                               long *stride, void *storage)
{
    OGRRecord *rec = (OGRRecord *) grpnode->other_src;

    FieldExtractor helper;
    long i, offset;

  /*
   *fprintf(stderr, "\nHit _getFieldVariable, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    if(NULL == varnode)
        return NULL;

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

/*
 * _getGeometryVariable()
 * 
 * Utility to return the contents of one of the geometry variables.
 *
 */
static void *_getGeometryVariable(NclFileGrpNode *grpnode,
                                  NclFileVarNode *varnode,
                                  long *start, long *finish,
                                  long *stride, void *storage)
{
    OGRRecord *rec = (OGRRecord *) grpnode->other_src;
    storage = _loadFeatureGeometry(rec, varnode, start, finish, stride);

    return storage;
}

/*
 * NewOGRInitializeFileRec()
 *
 */
static void *NewOGRInitializeFileRec(NclFileFormat *format)
{
    NclFileGrpNode *grpnode = NULL;

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tNewOGRInitializeFileRec...\n");
   */

    *format = _NclNewOGR;

    grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
    assert(grpnode);

    grpnode->id = -1;
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
 * NewOGROpenFile()
 *
 */
static void *NewOGROpenFile(void *therec, NclQuark path, int wr_status)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;

    OGRRecord *rec = NULL;
    OGRLayerH layer;
    OGRFeatureH feature;
    OGRGeometryH geom;
    OGRFeatureDefnH layerDefn;
    OGRwkbGeometryType geomType;
    OGRSpatialReferenceH sSrs;

    int numGeometry = 0;
    int numSegments = 0;
    int numPoints = 0;

  /*
   */
    fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tNewOGROpenFile...\n");

    rec = (OGRRecord*)NclCalloc(1, sizeof(OGRRecord));
    if(rec == NULL) {
    	return(NULL);
    }

    if(NULL == grpnode)
    {
        grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
        assert(grpnode);
    }

    grpnode->other_src = rec;

    rec->is3DGeometry = 0;

    if(!NewOGRInitialized)
    {
      /* Initialize the OGR drivers, and make sure OGR is one of them... */
        OGRRegisterAll();
        NewOGRInitialized = 1;
    }

  /*NOTE: for now we only support read-only access */
    if(wr_status == 0)
    {
        NhlPError(NhlWARNING, NhlEUNKNOWN,
                  "writing to OGR files is not currently implemented.");
        wr_status = 1;
    }

    grpnode->path = path;
    grpnode->status = wr_status;
    grpnode->id  = -1;
    grpnode->pid = -1;

    rec->dataSource = OGROpen(NrmQuarkToString(path), !wr_status, NULL);
    if (rec->dataSource == NULL)
    {
        NHLPERROR((NhlFATAL, NhlEUNKNOWN, "Failed to open OGR file: <%s>",
                   NrmQuarkToString(path)));
        return NULL;
    }       

  /*extract first feature of *FIRST* layer, which will be the basis for this 
   *file definition.
   */
    layer = OGR_DS_GetLayer(rec->dataSource, 0);
    if (layer == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "No layers found in OGR file:");
        NhlPError(NhlFATAL, NhlEUNKNOWN, NrmQuarkToString(path));
        return NULL;
    }

    layerDefn = OGR_L_GetLayerDefn(layer);
    rec->layer = layer;
    rec->layerDefn = layerDefn;

    rec->xform = NULL;
    sSrs = OGR_L_GetSpatialRef(rec->layer);
    if (sSrs != NULL && !OSRIsGeographic(sSrs))
    {
        OGRSpatialReferenceH tSrs = OSRNewSpatialReference(NULL); 
        OSRSetWellKnownGeogCS(tSrs, "WGS84");  /* LonLat */
        rec->xform = OCTNewCoordinateTransformation(sSrs, tSrs); 
    }

    geomType = OGR_FD_GetGeomType(layerDefn);
    if(_is3DGeometry(geomType))
    {
        rec->is3DGeometry = 1;
    }

  /*Read through the geometry to get the number of features, segments, and points...*/
    OGR_L_ResetReading(layer);
    while( (feature = OGR_L_GetNextFeature(layer)) != NULL )
    {
        geom = OGR_F_GetGeometryRef(feature);

        _countGeometry(geom, &numSegments, &numPoints);

        ++numGeometry;
      /*
       *OGR_F_Destroy(feature);

       *fprintf(stderr, "\tGeometry %d: segs: %d, points: %d\n",
       *                 numGeometry, numSegments, numPoints);
       */
    }

    grpnode->other_src = (void *)rec;

    _setGroupAtts(grpnode, layerDefn, numGeometry, numSegments, numPoints);
    _setGroupDims(grpnode, numGeometry, numSegments, numPoints);
    _setGroupVars(grpnode, layerDefn, numGeometry, numSegments, numPoints);

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tGeometry %d: segs: %d, points: %d\n",
   *                 numGeometry, numSegments, numPoints);
   */

    return((void*)grpnode);
}

/*
 * NewOGRCreateFile()
 *
 */
static void *NewOGRCreateFile(void *rec, NclQuark path)
{
    return(NewOGROpenFile(rec,path,-1));
}

/*
 * NewOGRReadVar()
 *
 */
static void *NewOGRReadVar(void* therec, NclQuark thevar,
                           long* start, long* finish, 
                           long* stride, void* storage)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    NclFileVarNode *varnode;

  /*
   *fprintf(stderr, "\nHit NewOGRReadVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   */

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);

    if(NULL != varnode)
    {
        /* found the requested variable... */
        if(NrmStringToQuark("Geometry") == thevar)
            return _getGeometryVariable(grpnode, varnode, start, finish, stride, storage);
        else
            return _getFieldVariable(grpnode, varnode, start, finish, stride, storage);
    }

    return NULL;
}

/*
 * NewOGRReadCoord()
 *
 */
static void *NewOGRReadCoord(void *therec, NclQuark thevar,
                             long *start, long *finish,
                             long *stride, void *storage)
{
    fprintf(stderr, "\nHit NewOGRReadCoord, file: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tNewOGRReadCoord...UNIMPLEMENTED\n");
    return(NewOGRReadVar(therec,thevar,start,finish,stride,storage));
}

NclFormatFunctionRec NewOGRRec = {
/* NclInitializeFileRecFunc initialize_file_rec */      NewOGRInitializeFileRec,
/* NclCreateFileFunc       create_file; */		NewOGRCreateFile,
/* NclOpenFileFunc         open_file; */		NewOGROpenFile,
/* NclFreeFileRecFunc      free_file_rec; */    	NULL,
/* NclGetVarNamesFunc      get_var_names; */    	NULL,
/* NclGetVarInfoFunc       get_var_info; */		NULL,
/* NclGetDimNamesFunc      get_dim_names; */    	NULL,
/* NclGetDimInfoFunc       get_dim_info; */		NULL,
/* NclGetAttNamesFunc      get_att_names; */    	NULL,
/* NclGetAttInfoFunc       get_att_info; */		NULL,
/* NclGetVarAttNamesFunc   get_var_att_names; */	NULL,
/* NclGetVarAttInfoFunc    get_var_att_info; */    	NULL,
/* NclGetCoordInfoFunc     get_coord_info; */    	NULL,
/* NclReadCoordFunc        read_coord; */		NewOGRReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */    		NewOGRReadVar,
/* NclReadVarFunc          read_var; */    		NULL,
/* NclReadAttFunc          read_att; */    		NULL,
/* NclReadVarAttFunc       read_var_att; */		NULL,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		NULL,
/* NclWriteVarAttFunc      write_var_att; */    	NULL,
/* NclAddDimFunc           add_dim; */    		NULL,
/* NclAddChunkDimFunc      add_chunk_dim; */    	NULL,
/* NclRenameDimFunc        rename_dim; */		NULL,
/* NclAddVarFunc           add_var; */    		NULL,
/* NclAddVarChunkFunc      add_var_chunk; */    	NULL,
/* NclAddVarChunkCacheFunc add_var_chunk_cache; */	NULL,
/* NclSetVarCompressLevelFunc set_var_compress_level;*/ NULL,
/* NclAddVarFunc           add_coord_var; */    	NULL,
/* NclAddAttFunc           add_att; */    		NULL,
/* NclAddVarAttFunc        add_var_att; */		NULL,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	NULL,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	NULL,
/* NclDelAttFunc           del_att; */    		NULL,
/* NclDelVarAttFunc        del_var_att; */		NULL,
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
/* NclSetOptionFunc        set_option;  */              NULL
};


/*
 * OGRAddFileFormat()
 *
 */
NclFormatFunctionRecPtr NewOGRAddFileFormat(void)
{    
    return(&NewOGRRec);
}

