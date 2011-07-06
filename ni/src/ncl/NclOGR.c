/*
 *      $Id: NclOGR.c,v 1.5 2010-05-06 22:52:28 huangwei Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  2009			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:	        March, 2009
 *
 *	Description:	
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
 *          geometry(*, 0) = index into segments table of first segment for the feature
 *          geometry(*, 1) = the number of segments belonging to this feature.
 *
 *      The "segments" table is a partially ordered list of segments (individual points,
 *      polylines, polygons). There are one or more entries in this table per feature; the 
 *      geometry table points to the first segment entry, and all entries belonging to 
 *      the feature directly follow the first. Entries in the segments table provide pointers
 *      to the xy(z) coordinates:
 *          segments(*, 0) = index into X, Y, Z arrays of first point of the segment
 *          segments(*, 1) = number of points in the segment.
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
#include <ogr_api.h>
#include <ogr_srs_api.h>
#include <math.h>
#include <unistd.h>

/* #define OGR_DEBUG 1 */

typedef struct _OGRRecord OGRRecord;


/* Our Private File Record */
struct _OGRRecord {
        NclQuark         file_path_q;
        int              wr_status;
        OGRDataSourceH   dataSource;
        OGRLayerH        layer;
        OGRFeatureDefnH  layerDefn;
        OGRCoordinateTransformationH xform;
        NclFDimRec       *dimensions;
        int              numDimensions;

        NclFVarRec       *variables;
        int              numVariables;

        NclFAttRec       *globalAtts;
        long             *globalAttsValues;
        int              numAtts;

        /* pointers to the contents of the "geometry" variables */
        int              *geometry;
        int              *segments;
        double           *x;
        double           *y;
        double           *z;

        int              is3DGeometry;
        int              numGeomVariables;
};

/* numerical indices for the geometry variables */
#define GEOMVAR_GEOMETRY 0
#define GEOMVAR_SEGMENTS 1
#define GEOMVAR_X        2
#define GEOMVAR_Y        3
#define GEOMVAR_Z        4

/* numerical indices for the dimensions */
#define DIM_GEOMETRY     0
#define DIM_SEGMENTS     1
#define DIM_NUM_FEATURES 2
#define DIM_NUM_SEGMENTS 3
#define DIM_NUM_POINTS   4

/* constants for number of columns in the geometry and segments tables */
#define NUM_GEOMETRY_COLS 2
#define NUM_SEGMENTS_COLS 2

static int OGRInitialized = 0;

/* these macros provide convenient indexing into geometry & segments tables */
#define GEOMIDX(feature, i) (feature*NUM_GEOMETRY_COLS + i)
#define SEGIDX(segment, i)  (segment*NUM_SEGMENTS_COLS + i)

/* symbolic constants for use as second index into geometry and segments tables */
#define GEOM_SEGINDEX 0
#define GEOM_NUMSEGS  1
#define SEGS_XYZINDEX 0
#define SEGS_NUMXYZ   1

/*
 * _is3DGeometry()
 *
 */
static int _is3DGeometry
#if     NhlNeedProto
(OGRwkbGeometryType geom)
#else
(geom)
OGRwkbGeometryType geom;
#endif
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
	        default:          return NCL_none;
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
 * _defineDimensions()
 *
 * Creates dimension records for our fixed set of dimensions.
 *
 */
static void _defineDimensions
#if	NhlNeedProto
(OGRRecord *rec)
#else
(rec)
OGRRec *rec;
#endif
{
        /* Define our fixed set of dimensions */
        rec->dimensions = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec) * 5);

        /* the per-feature dimension in our geometry table */
        rec->dimensions[0].dim_name_quark = NrmStringToQuark("geometry");
        rec->dimensions[0].dim_size = NUM_GEOMETRY_COLS;
        rec->dimensions[0].is_unlimited = 0;

        /* the per-segment dimension in our segments table */
        rec->dimensions[1].dim_name_quark = NrmStringToQuark("segments");
        rec->dimensions[1].dim_size = NUM_SEGMENTS_COLS;
        rec->dimensions[1].is_unlimited = 0;

        /* the per-feature dimension  (fill in size later) */
        rec->dimensions[2].dim_name_quark = NrmStringToQuark("num_features");
        rec->dimensions[2].dim_size = 0;
        rec->dimensions[2].is_unlimited = 1;

        /* the segments (fill in size later) */
        rec->dimensions[3].dim_name_quark = NrmStringToQuark("num_segments");
        rec->dimensions[3].dim_size = 0;
        rec->dimensions[3].is_unlimited = 0;

        /* the XY(Z) points (fill in size later) */
        rec->dimensions[4].dim_name_quark = NrmStringToQuark("num_points");
        rec->dimensions[4].dim_size = 0;
        rec->dimensions[4].is_unlimited = 0;

        rec->numDimensions = 5;
} 


/*
 * _defineGlobalAttributes()
 *
 * Defines attribute records for our fixed set of global attributes.
 *
 */
static void _defineGlobalAttributes
#if	NhlNeedProto
(OGRRecord *rec)
#else
(rec)
OGRRec *rec;
#endif
{
        int i=0;

        /* Define the global attributes... */
        rec->globalAtts = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec) * 7);
        rec->globalAttsValues = NclMalloc(sizeof(long) * 7);

        /* the layer name */        
        rec->globalAtts[i].att_name_quark = NrmStringToQuark("layer_name");
        rec->globalAtts[i].data_type = NCL_string;
        rec->globalAtts[i].num_elements = 1;
        rec->globalAttsValues[i] =  NrmStringToQuark( OGR_FD_GetName(rec->layerDefn) );
        ++i;

        /* the geometry-type of the layer */
        rec->globalAtts[i].att_name_quark = NrmStringToQuark("geometry_type");
        rec->globalAtts[i].data_type = NCL_string;
        rec->globalAtts[i].num_elements = 1;
        rec->globalAttsValues[i] =  NrmStringToQuark(
            _mapOGRGeom2Ncl(OGR_FD_GetGeomType(rec->layerDefn)) );
        ++i;

        /* The remaining global-atts are "convenience constants" intended to be used 
         * in scripting code to index into the geometry and segments variables.
         */

        rec->globalAtts[i].att_name_quark = NrmStringToQuark("geom_segIndex");
        rec->globalAtts[i].data_type = NCL_int;
        rec->globalAtts[i].num_elements = 1;
        rec->globalAttsValues[i] = 0;   /* index zero */
        ++i;

        rec->globalAtts[i].att_name_quark = NrmStringToQuark("geom_numSegs");
        rec->globalAtts[i].data_type = NCL_int;
        rec->globalAtts[i].num_elements = 1;
        rec->globalAttsValues[i] = 1;
        ++i;

        rec->globalAtts[i].att_name_quark = NrmStringToQuark("segs_xyzIndex");
        rec->globalAtts[i].data_type = NCL_int;
        rec->globalAtts[i].num_elements = 1;
        rec->globalAttsValues[i] = 0;
        ++i;

        rec->globalAtts[i].att_name_quark = NrmStringToQuark("segs_numPnts");
        rec->globalAtts[i].data_type = NCL_int;
        rec->globalAtts[i].num_elements = 1;
        rec->globalAttsValues[i] = 1;
        ++i;

        rec->numAtts = i;
}


/*
 * _defineVariables()

 * Defines variables records for this OGR file. There is a predetermined set of variables, 
 * referred to collectively as the "geometry variables" ("geometry", "segments", 
 * "x", "y", and optionally "z"). Then we create variables for each (non-spatial) field
 * associated with records in the OGR file.
 *
 */
static void _defineVariables
#if	NhlNeedProto
(OGRRecord *rec)
#else
(rec)
OGRRec *rec;
#endif
{
        OGRwkbGeometryType geomType;
        int numVars;
        int i = 0, j = 0;

        /* We have a fixed set of variables that are common to all OGR datasets, 
         * plus a set that are specific to the given dataset.
         */
        
        numVars = OGR_FD_GetFieldCount(rec->layerDefn);
 	geomType = OGR_FD_GetGeomType(rec->layerDefn);
        rec->numGeomVariables = 4;  /* minimally, the "geometry", "segments" "x" "y" vars */
        rec->is3DGeometry = 0;
        if (_is3DGeometry(geomType)) {
                rec->numGeomVariables++;  /* add 1 for a "z" variable */
                rec->is3DGeometry = 1;
        }

        rec->variables = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec) * (numVars + rec->numGeomVariables));

        /* the "geometry" variable encodes each distinct feature in the file  */        
        rec->variables[i].var_name_quark = NrmStringToQuark("geometry");
        rec->variables[i].data_type = NCL_int;
        rec->variables[i].num_dimensions = 2;
        rec->variables[i].file_dim_num[0] = 2;  /* MUST ALIGN WITH DIMENSION DEFS */
        rec->variables[i].file_dim_num[1] = 0;  /* MUST ALIGN WITH DIMENSION DEFS */
        ++i;

        /* the "segments" variable encodes the geometry of the features  */        
        rec->variables[i].var_name_quark = NrmStringToQuark("segments");
        rec->variables[i].data_type = NCL_int;
        rec->variables[i].num_dimensions = 2;
        rec->variables[i].file_dim_num[0] = 3;  /* MUST ALIGN WITH DIMENSION DEFS */
        rec->variables[i].file_dim_num[1] = 1;  /* MUST ALIGN WITH DIMENSION DEFS */
        ++i;

        /* the X variable  */        
        rec->variables[i].var_name_quark = NrmStringToQuark("x");
        rec->variables[i].data_type = NCL_double;
        rec->variables[i].num_dimensions = 1;
        rec->variables[i].file_dim_num[0] = 4;  /* MUST ALIGN WITH DIMENSION DEFS */
        ++i;

        /* the Y variable  */        
        rec->variables[i].var_name_quark = NrmStringToQuark("y");
        rec->variables[i].data_type = NCL_double;
        rec->variables[i].num_dimensions = 1;
        rec->variables[i].file_dim_num[0] = 4;  /* MUST ALIGN WITH DIMENSION DEFS */
        ++i;

        if (rec->is3DGeometry) {
                /* the Z variable  */        
                rec->variables[i].var_name_quark = NrmStringToQuark("z");
                rec->variables[i].data_type = NCL_double;
                rec->variables[i].num_dimensions = 1;
                rec->variables[i].file_dim_num[0] = 4;  /* MUST ALIGN WITH DIMENSION DEFS */
                ++i;
        }

        /* March through the layer definition to get the dataset-specific variable defns.
         * Note that these variables exist 1:1 with features, and are thus same length as 
         * the geometry table.
         */
        for (j=0; j<numVars; j++) {
                OGRFieldDefnH fldDef = OGR_FD_GetFieldDefn(rec->layerDefn, j);
                rec->variables[i].var_name_quark = NrmStringToQuark( OGR_Fld_GetNameRef(fldDef) );
                rec->variables[i].data_type = _mapOGRType2Ncl(OGR_Fld_GetType(fldDef));
                rec->variables[i].num_dimensions = 1;
                rec->variables[i].file_dim_num[0] = 2;  /* MUST ALIGN WITH DIMENSION DEFS */
                ++i;
        }

        rec->numVariables = i;
}


/*
 * _countGeometry()
 *
 * Utility function used to recursively traverse OGRGeometry to obtain a count
 * of the total number of line-segments and numbers of XY(Z) tuples.
 *
 */
static void _countGeometry
#if	NhlNeedProto
(OGRGeometryH geom, ng_size_t *numSegments, ng_size_t *numPoints)
#else
(geom, numSegments, numPoints)
OGRGeometryH geom;
ng_size_t    *numSegments;
ng_size_t    *numPoints;
#endif
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
 * _loadFeatureGeometry()
 *
 * Utility function used to recursively load OGRGeometry.
 *
 */
static void _loadFeatureGeometry
#if	NhlNeedProto
(OGRRecord *rec, OGRGeometryH geom, int *numSegments, int *numPoints)
#else
(rec, geom, numSegments, numPoints)
OGRRecord *rec;
OGRGeometryH    geom;
int             *numSegments;
int             *numPoints;
#endif
{       int i;



                
        int geomCount = OGR_G_GetGeometryCount(geom);
        if (geomCount == 0) {
                if (rec->xform) {
                        OGRErr err = OGR_G_Transform(geom, rec->xform);
                }
                int numSegPts = OGR_G_GetPointCount(geom);
                rec->segments[SEGIDX(*numSegments, SEGS_XYZINDEX)] = *numPoints;
                rec->segments[SEGIDX(*numSegments, SEGS_NUMXYZ)] = numSegPts;
                
                for (i=0; i<numSegPts; i++) {
                        rec->x[*numPoints] = OGR_G_GetX(geom, i);
                        rec->y[*numPoints] = OGR_G_GetY(geom, i);
                        if (rec->is3DGeometry)
                                rec->z[*numPoints] = OGR_G_GetZ(geom, i);
                        (*numPoints)++;
                }

                (*numSegments)++;
        }
        else {
                /* compound geometry */
                OGRGeometryH subGeom;
                int i;  
                for (i=0; i<geomCount; i++) {
                        subGeom = OGR_G_GetGeometryRef(geom, i);
                         _loadFeatureGeometry(rec, subGeom, numSegments, numPoints);
                }
        }
}


/*
 * _loadGeometry()
 *
 * Intended to be called once to load and cache all of the geometry variables, whenever
 * any one of them is asked for.
 *
 */
static int _loadGeometry
#if	NhlNeedProto
(OGRRecord *rec)
#else
(rec)
OGRRecord *rec;
#endif
{
        OGRFeatureH feature;
        int featureNum = 0;
        int segmentNum = 0;
        int pointNum = 0;

        if (rec->geometry != NULL)
                return 1;   /* already loaded */

        rec->geometry = (int*) NclMalloc(sizeof(int) * NUM_GEOMETRY_COLS * 
            rec->dimensions[DIM_NUM_FEATURES].dim_size);
        rec->segments = (int*) NclMalloc(sizeof(int) * NUM_SEGMENTS_COLS * 
            rec->dimensions[DIM_NUM_SEGMENTS].dim_size);
        rec->x = (double*) NclMalloc(sizeof(double) * rec->dimensions[DIM_NUM_POINTS].dim_size);
        rec->y = (double*) NclMalloc(sizeof(double) * rec->dimensions[DIM_NUM_POINTS].dim_size);
        if (rec->is3DGeometry) 
                rec->z = (double*) NclMalloc(sizeof(double) * rec->dimensions[DIM_NUM_POINTS].dim_size);
        if (rec->geometry == NULL || rec->segments == NULL || rec->x == NULL ||
            rec->y == NULL || (rec->is3DGeometry && rec->z == NULL))
        {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return 0;
        }

        OGR_L_ResetReading(rec->layer);
        while( (feature = OGR_L_GetNextFeature(rec->layer)) != NULL ) {
                OGRGeometryH geom = OGR_F_GetGeometryRef(feature);
                rec->geometry[GEOMIDX(featureNum, GEOM_SEGINDEX)] = segmentNum;

                _loadFeatureGeometry(rec, geom, &segmentNum, &pointNum);
          
                rec->geometry[GEOMIDX(featureNum, GEOM_NUMSEGS)] = 
                    segmentNum - rec->geometry[GEOMIDX(featureNum, GEOM_SEGINDEX)];
                OGR_F_Destroy(feature);
                ++featureNum;
        }

        return 1;
}


/*
 * _getGeometryVariable()
 * 
 * Utility to return the contents of one of the geometry variables.
 *
 */
static void *_getGeometryVariable
#if	NhlNeedProto
(OGRRecord *rec, int varNum, long *start, long *finish, long *stride, void *storage)
#else
(rec, varNum, start, finish, stride, storage)
OGRRecord *rec;
int varNum;
long start;
long finish;
long stride;
void *storage;
#endif
{
        /* On first innvocation, we'll load and cache all of the geometry variables,
         * under the premise that its quite likely a request for any of them is part of
         * a broader request for the geometry as a whole.
         */
        if (!_loadGeometry(rec))
                return NULL;

        /* NOTE that we use explicit knowledge of the dimensions of these variables */

        if (varNum == GEOMVAR_X) {
                int i;
                for (i=start[0]; i<=finish[0]; i+=stride[0]) {
                        ((double*)storage)[i-start[0]] = rec->x[i];
                }
        }

        else if (varNum == GEOMVAR_Y) {
                int i;
                for (i=start[0]; i<=finish[0]; i+=stride[0]) {
                        ((double*)storage)[i-start[0]] = rec->y[i];
                }
        }

        else if (varNum == GEOMVAR_Z) {
                int i;
                for (i=start[0]; i<=finish[0]; i+=stride[0]) {
                        ((double*)storage)[i-start[0]] = rec->z[i];
                }
        }

        else if (varNum == GEOMVAR_GEOMETRY) {
                int i, j;
                int cols = (finish[1]-start[1]+1) / stride[1];
                for (i=start[0]; i<=finish[0]; i+=stride[0]) {
                        for (j=start[1]; j<=finish[1]; j+=stride[1]) {
                                *( ((int*)storage) + (i-start[0])*cols  + (j-start[1])) = 
                                    *(rec->geometry + i*NUM_GEOMETRY_COLS + j);
                        }
                }
        }

        else if (varNum == GEOMVAR_SEGMENTS) {
                int i, j;
                int cols = (finish[1]-start[1]+1) / stride[1];
                for (i=start[0]; i<=finish[0]; i+=stride[0]) {
                        for (j=start[1]; j<=finish[1]; j+=stride[1]) {
                                *( ((int*)storage) + (i-start[0])*cols  + (j-start[1])) = 
                                    *(rec->segments + i*NUM_SEGMENTS_COLS + j);
                        }
                }
        }

        else
                return NULL;

        return storage;
}


/*
 * When it comes to extracting field variables, we set a pointer to a function of
 * this type that can extract and return a field of the proper NCL-type.
 */
typedef void (*FieldExtractor)(
#if	NhlNeedProto
OGRFeatureH feature, int fieldNum, void* storage, long offset
#endif
);


/*
 * _getFieldAsInteger()
 *
 * A FieldExtractor for integer fields.
 */
static void _getFieldAsInteger
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
static void *_getFieldVariable
#if	NhlNeedProto
(OGRRecord *rec, int varNum, long *start, long *finish, long *stride, void *storage)
#else
(rec, varNum, start, finish, stride, storage)
OGRRecord *rec;
int varNum;
long start;
long finish;
long stride;
void *storage;
#endif
{
        FieldExtractor helper;
        long i, offset;
        int fieldNum = varNum - GEOMVAR_Z;

        /* Based upon the type of the variable we're after, we'll invoke a different 
         * helper function in the loop below.  Note that the caller has already 
         * verified varNum as a valid index.
         */
        switch (rec->variables[varNum].data_type) {
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
        for (i=start[0]; i<= finish[0]; i+=stride[0]) {
                OGRFeatureH feature = OGR_L_GetFeature(rec->layer, i);
                
                /* get the field corresponding to varNum. */
                (*helper)(feature, fieldNum, storage, offset++);
              
                OGR_F_Destroy(feature);
        }

        return storage;
}


/*
 * OGRInitializeFileRec()
 *
 */
static void *OGRInitializeFileRec
#if	NhlNeedProto
(NclFileFormat *format)
#else
(format)
NclFileFormatType *format;
#endif
{
        OGRRecord*  therec = (OGRRecord*) NULL;
       
        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRInitializeFileRec...\n");
        #endif

	therec = (OGRRecord*)NclCalloc(1, sizeof(OGRRecord));
	if (! therec) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}

	*format = _NclOGR;
	return (void *) therec;
}


/*
 * OGROpenFile()
 *
 */
static void *OGROpenFile
#if	NhlNeedProto
(void *therec, NclQuark path, int wr_status)
#else
(rec,path,wr_status)
void *rec;
NclQuark path;
int wr_status;
#endif
{
	OGRRecord *rec = (OGRRecord*) therec;
        OGRLayerH layer;
        OGRFeatureH feature;
        OGRSpatialReferenceH sSrs;
        ng_size_t numGeometry = 0;
        ng_size_t numSegments = 0;
        ng_size_t numPoints = 0;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGROpenFile...\n");
        #endif

	if(rec == NULL) {
		return(NULL);
	}

        if (!OGRInitialized) {
                /* Initialize the OGR drivers, and make sure OGR is one of them... */
                OGRRegisterAll();
                OGRInitialized = 1;
        }

        /* NOTE: for now we only support read-only access */
        if (wr_status == 0) {
                NhlPError(NhlWARNING, NhlEUNKNOWN,
                    "writing to OGR files is not currently implemented.");
                wr_status = 1;
        }

        rec->dataSource = OGROpen(NrmQuarkToString(path), !wr_status, NULL);
        if (rec->dataSource == NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN, "Failed to open OGR file: ");
                NhlPError(NhlFATAL, NhlEUNKNOWN, NrmQuarkToString(path));
                return NULL;
        }       
	rec->file_path_q = path;
	rec->wr_status = wr_status;

        /* extract first feature of *FIRST* layer, which will be the basis for this 
         * file definition.
         */
        layer = OGR_DS_GetLayer(rec->dataSource, 0);
        if (layer == NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN, "No layers found in OGR file:");
                NhlPError(NhlFATAL, NhlEUNKNOWN, NrmQuarkToString(path));
                return NULL;
        }
        rec->layerDefn = OGR_L_GetLayerDefn(layer);
        rec->layer = layer;

        rec->xform = NULL;
        sSrs = OGR_L_GetSpatialRef(rec->layer);
        if (sSrs != NULL && !OSRIsGeographic(sSrs)) {
                OGRSpatialReferenceH tSrs = OSRNewSpatialReference(NULL); 
                OSRSetWellKnownGeogCS(tSrs, "WGS84");  /* LonLat */
                rec->xform = OCTNewCoordinateTransformation(sSrs, tSrs); 
        }
        _defineDimensions(rec);
        _defineGlobalAttributes(rec);
        _defineVariables(rec);


        /* Read through the geometry to get the number of features, segments, and points... */
        OGR_L_ResetReading(layer);
        while( (feature = OGR_L_GetNextFeature(layer)) != NULL ) {
                OGRGeometryH geom = OGR_F_GetGeometryRef(feature);
                _countGeometry(geom, &numSegments, &numPoints);
                OGR_F_Destroy(feature);
                ++numGeometry;
        }

        /* update our remaining dimensions... */
        rec->dimensions[2].dim_size = numGeometry;
        rec->dimensions[3].dim_size = numSegments;
        rec->dimensions[4].dim_size = numPoints;

	return((void*)rec);
}


/*
 * OGRCreateFile()
 *
 */
static void *OGRCreateFile
#if	NhlNeedProto
(void *rec,NclQuark path)
#else
(rec,path)
void *rec;
NclQuark path;
#endif
{
	OGRRecord *tmp = (OGRRecord*)rec;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRCreateFile...\n");
        #endif

        return((void*) tmp);
}


/*
 * OGRFreeFileRec()
 *
 */
static void OGRFreeFileRec
#if	NhlNeedProto
(void* therec)
#else
(therec)
void *therec;
#endif
{
	OGRRecord *rec = (OGRRecord*)therec;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRFreeFileRec...\n");
        #endif

        if (rec == NULL)
                return;

        if (rec->dimensions) NclFree(rec->dimensions);     
        if (rec->globalAtts) NclFree(rec->globalAtts);
        if (rec->globalAttsValues) NclFree(rec->globalAttsValues);
        if (rec->variables)  NclFree(rec->variables);

        if (rec->geometry) NclFree(rec->geometry);
        if (rec->segments) NclFree(rec->segments);
        if (rec->x) NclFree(rec->x);
        if (rec->y) NclFree(rec->y);
        if (rec->is3DGeometry && rec->z) NclFree(rec->z);

        OGR_DS_Destroy(rec->dataSource);

        NclFree(rec);

	return;
}


/*
 * OGRGetVarNames()
 *
 */
static NclQuark* OGRGetVarNames
#if	NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
        OGRRecord *rec = (OGRRecord*)therec;
        int i;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRGetVarNames...\n");
        #endif

        NclQuark* names = (NclQuark*) NclMalloc(sizeof(NclQuark) * rec->numVariables);
        if (names == NULL) {
                NhlPError(NhlFATAL,ENOMEM,NULL);
                return NULL;
        }

        for (i=0; i<rec->numVariables; i++) {
                names[i] = rec->variables[i].var_name_quark;
        }

        *num_vars = rec->numVariables;
        return names;
}


/*
 * OGRGetVarInfo()
 *
 */
static NclFVarRec *OGRGetVarInfo
#if	NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif
{
	OGRRecord *rec = (OGRRecord*)therec;
        int i=0;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRGetVarInfo...\n");	
        #endif
        
        for (i=0; i<rec->numVariables; i++) {
                if (var_name == rec->variables[i].var_name_quark) {
                        int j;
                        NclFVarRec *ret = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec));
                        ret->var_name_quark = rec->variables[i].var_name_quark;
                        ret->var_full_name_quark = rec->variables[i].var_name_quark;
                        ret->var_real_name_quark = rec->variables[i].var_name_quark;
                        ret->data_type = rec->variables[i].data_type;
                        ret->num_dimensions = rec->variables[i].num_dimensions;
                        for (j=0; j<ret->num_dimensions; j++)
                                ret->file_dim_num[j] = rec->variables[i].file_dim_num[j];
                        return ret;
                }
        }

        return NULL;
}


/*
 * OGRGetDimNames()
 *
 */
static NclQuark *OGRGetDimNames
#if	NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
	OGRRecord *rec = (OGRRecord*)therec;
        int i;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRGetDimNames...\n");
        #endif

        NclQuark* names = (NclQuark*) NclMalloc(sizeof(NclQuark) * rec->numDimensions);
        if (names == NULL) {
                NhlPError(NhlFATAL,ENOMEM,NULL);
                return NULL;
        }

        for (i=0; i<rec->numDimensions; i++) {
                names[i] = rec->dimensions[i].dim_name_quark;
        }

        *num_dims = rec->numDimensions;
        return names;        
}


/*
 * OGRGetDimInfo()
 *
 */
static NclFDimRec *OGRGetDimInfo
#if	NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
	OGRRecord* rec = (OGRRecord*)therec;
        int i;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRGetDimInfo...\n");
        #endif

        for (i=0; i<rec->numDimensions; i++) {
                if (dim_name_q == rec->dimensions[i].dim_name_quark) {
                        NclFDimRec* ret = (NclFDimRec*) NclMalloc(sizeof(NclFDimRec));
                        *ret = rec->dimensions[i];
                        return ret;
                }
        }

	return NULL;
}


/*
 * OGRGetAttNames()
 *
 */
static NclQuark *OGRGetAttNames
#if	NhlNeedProto
(void* therec, int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{	
	OGRRecord* rec = (OGRRecord*)therec;
        int i;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRGetAttNames...\n");
        #endif

        NclQuark* names = (NclQuark*) NclMalloc(sizeof(NclQuark) * rec->numAtts);
        if (names == NULL) {
                NhlPError(NhlFATAL,ENOMEM,NULL);
                return NULL;
        }

        for (i=0; i<rec->numAtts; i++) {
                names[i] = rec->globalAtts[i].att_name_quark;
        }

        *num_atts = rec->numAtts;
        return names;
}


/* 
 * OGRGetAttInfo()
 *
 */
static NclFAttRec* OGRGetAttInfo
#if	NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
	OGRRecord* rec = (OGRRecord*)therec;
        int i;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRGetAttInfo...\n");
        #endif

        for (i=0; i<rec->numAtts; i++) {
                if (att_name_q == rec->globalAtts[i].att_name_quark) {
                        NclFAttRec* ret = (NclFAttRec*) NclMalloc(sizeof(NclFAttRec));
                        *ret = rec->globalAtts[i];
                        return ret;
                }
        }

	return NULL;
}


/*
 * OGRGetVarAttNames()
 *
 */
static NclQuark *OGRGetVarAttNames
#if	NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRGetVarAttNames...\n");
        #endif

        *num_atts = 0;
	return(NULL);
}


/*
 * OGRGetVarAttInfo()
 *
 */
static NclFAttRec *OGRGetVarAttInfo
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRGetVarAttInfo...\n");
        #endif
		
	return(NULL);
}


/*
 * OGRGetCoordInfo()
 *
 */
static NclFVarRec *OGRGetCoordInfo
#if	NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRGetCoordInfo...\n");
        #endif
	return NULL;
}


/*
 * OGRReadVar()
 *
 */
static void *OGRReadVar
#if	NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish, long* stride, void* storage)
#else
(therec, thevar, start, finish,stride,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
	OGRRecord *rec = (OGRRecord*) therec;
        int i;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRReadVar...\n");
        #endif

        for (i=0; i<rec->numVariables; i++) {
                if (thevar != rec->variables[i].var_name_quark)
                        continue;

                /* found the requested variable... */
                if (i < rec->numGeomVariables) 
                        return _getGeometryVariable(rec, i, start, finish, stride, storage);
                else
                        return _getFieldVariable(rec, i, start, finish, stride, storage);
        }

	return NULL;
}


/* 
 * OGRReadCoord()
 *
 */
static void *OGRReadCoord
#if	NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish, long* stride, void* storage)
#else
(therec, thevar, start, finish,stride,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRReadCoord...UNIMPLEMENTED\n");
        #endif
	return(OGRReadVar(therec,thevar,start,finish,stride,storage));
}


/*
 * OGRReadAtt()
 *
 */
static void *OGRReadAtt
#if	NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
	OGRRecord *rec = (OGRRecord*)therec;
        int i;

        #ifdef OGR_DEBUG
        fprintf(stderr, "OGRReadAtt...\n");
        #endif

        for (i=0; i<rec->numAtts; i++) {
                if (theatt == rec->globalAtts[i].att_name_quark) {
                        /*if (rec->globalAtts[i].data_type == NCL_char)
                        //        *(char*)storage = (char*)rec->globalAttsValues[i];
                        //0else*/
		  /* *storage = rec->globalAttsValues[i];  */
                	if (rec->globalAtts[i].data_type == NCL_string)
                		*(long*) storage = rec->globalAttsValues[i];
                	else
                		*(int*) storage = (int) rec->globalAttsValues[i];
					return (storage);
                }
        }

	return NULL;
}

NclFormatFunctionRec OGRRec = {
/* NclInitializeFileRecFunc initialize_file_rec */      OGRInitializeFileRec,
/* NclCreateFileFunc	   create_file; */		OGRCreateFile,
/* NclOpenFileFunc         open_file; */		OGROpenFile,
/* NclFreeFileRecFunc      free_file_rec; */		OGRFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		OGRGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		OGRGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		OGRGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		OGRGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		OGRGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		OGRGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	OGRGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		OGRGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		OGRGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		OGRReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			OGRReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			OGRReadAtt,
/* NclReadVarAttFunc       read_var_att; */		NULL,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		NULL,
/* NclWriteVarAttFunc      write_var_att; */		NULL,
/* NclAddDimFunc           add_dim; */			NULL,
/* NclAddChunkDimFunc      add_chunk_dim; */		NULL,
/* NclRenameDimFunc        rename_dim; */		NULL,
/* NclAddVarFunc           add_var; */			NULL,
/* NclAddVarChunkFunc      add_var_chunk; */		NULL,
/* NclAddVarChunkCacheFunc add_var_chunk_cache; */	NULL,
/* NclSetVarCompressLevelFunc set_var_compress_level; */ NULL,
/* NclAddVarFunc           add_coord_var; */		NULL,
/* NclAddAttFunc           add_att; */			NULL,
/* NclAddVarAttFunc        add_var_att; */		NULL,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	NULL,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	NULL,
/* NclDelAttFunc           del_att; */			NULL,
/* NclDelVarAttFunc        del_var_att; */		NULL,
#include "NclGrpFuncs.null"
/* NclSetOptionFunc        set_option;  */              NULL
};


/*
 * OGRAddFileFormat()
 *
 */
NclFormatFunctionRecPtr OGRAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{	
	return(&OGRRec);
}
