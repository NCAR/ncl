
#define NclAPICREATED 01
#define NclAPIDESTROYED 02
#define NclAPIMODIFIED 04

typedef enum  {
NCLNone =                      00,
NCLObj = 			01,
/* NCLData = 			02, */
/* NCLMultiDValData = 		04, */
/* NCLTypedouble =		010, */
/* NCLTypefloat = 	  	020, */
/* NCLTypelong = 	  		040, */
/* NCLTypeint = 		 	0100, */
/* NCLTypeshort = 	 	0200, */
/* NCLTypebyte = 	 		0400, */
/* NCLTypestring = 		01000, */
/* NCLTypechar= 			02000, */
/* NCLTypeobj= 			04000, */
NCLVar = 			010000,
/* NCLAtt = 			020000, */
/* NCLTypelogical =    		040000, */
NCLHLUObj = 			0100000,
NCLFile = 			0200000,
NCLFileVar = 			0400000,
NCLHLUVar = 			01000000
/* NCLCoordVar = 			02000000, */
/* NCLType = 			04000000, */
/* NCLMultiDValnclfileData =	010000000, */
/* NCLMultiDValHLUObjData = 	020000000, */
/* NCLOneDValCoordData = 		040000000 */
} NclApiObjTypes;

typedef enum { NclAPINORMAL, NclAPIVARSUBSEL , NclAPICOORD, NclAPICOORDSUBSEL, NclAPIFILEVAR, NclAPIFILEVARSUBSEL,NclAPIPARAM,NclAPIRETURNVAR,NclAPIHLUOBJ,NclAPIFUNCNORMAL } NclApiVarTypes;


typedef struct _NclObjInfoRec {
	int obj_id;
	NclApiObjTypes obj_type;
}NclObjInfoRec;

typedef struct _NclObjClassInfo {
	NclObjInfoRec obj;
}NclObjClassInfo;


typedef struct _NclVarInfoRec {
	NclApiVarTypes 	var_type;
	NclQuark 	var_quark;
	int		n_dims;
	int		dim_sizes[NCL_MAX_DIMENSIONS];
	int		dim_quarks[NCL_MAX_DIMENSIONS];
} NclVarInfoRec;

typedef struct _NclVarClassInfo {
	NclObjInfoRec obj;
	NclVarInfoRec var;
} NclVarClassInfo;

typedef struct _NclHLUVarInfoRec {
	struct _NclHLUObjInfoRec *the_hlu_info;
} NclHLUVarInfoRec;

typedef struct _NclHLUVarClassInfo {
	NclObjInfoRec obj;
        NclVarInfoRec var;
	NclHLUVarInfoRec hlu;
} NclHLUVarClassInfo;

typedef struct _NclFileVarInfoRec {
	int foo;
} NclFileVarInfoRec;

typedef struct _NclFileVarClassInfo {
	NclObjInfoRec obj;
        NclVarInfoRec var;
	NclFileVarInfoRec file;
} NclFileVarClassInfo;

typedef struct _NclFileInfoRec {
	NclQuark fname;
	NclQuark fpath;
	int	wr_status;
}NclFileInfoRec;

typedef struct _NclFileClassInfo {
	NclObjInfoRec obj;
	NclFileInfoRec file;
}NclFileClassInfo; 

typedef struct _NclHLUObjInfoRec {
	int hlu_id;
	NclQuark hlu_name;
	int parent_hluobj_id;
	NhlClass class_ptr;
}NclHLUObjInfoRec;

typedef struct _NclHLUObjClassInfo{
	NclObjInfoRec obj;
	NclHLUObjInfoRec hluobj;
} NclHLUObjClassInfo;

extern NhlErrorTypes NclApiRegisterCallback(
#if     NhlNeedProto
NclApiObjTypes /* obj_type */,
unsigned int /* type */, 
void* /* callback_function */, 
void* /* user_data */
#endif
); 
