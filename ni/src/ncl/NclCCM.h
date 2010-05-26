typedef struct ccmi {
	int LENHDI;
	int MFTYP;
	int MFILH;
	int MFILTH;
	int NRBD;
	int MAXSIZ;
	int NDAVU;
	int unused;
	int NLON;
	int NLONW;
	int NOREC;
	int NLEV;
	int NTRM;
	int NTRN;
	int NTRK;
	int NFLDH;
	int NSTEPH;
	int NSTPRH;
	int NITSLF;
	int NDBASE;
	int NSBASE;
	int NDCUR;
	int NSCUR;
	int NBDATE;
	int NBSEC;
	int NCDATE;
	int NCSEC;
	int MDT;
	int MHISF;
	int MFSTRT;
	int LENHDC;
	int LENHDR;
	int MPSIG;
	int MPLAT;
	int MPWTS;
	int MPFLDS;
	int MPCFLD;
	int *MFLDS;
}CCMI;
typedef struct ccmc {
	char MCASE[8];
	char MCSTIT[80];
	char LNHSTC[80];
	char LDHSTC[8];
	char LTHSTC[8];
	char LSHSTC[8];
	char LNHSTF[80];
	char LDHSTF[8];
	char LTHSTF[8];
	char LSHSTF[8];
	char LNHSTI[80];
	char LDHSTI[8];
	char LTHSTI[8];
	char LSHSTI[8];
	char LNHSTT[80];
	char LDHSTT[8];
	char LTHSTT[8];
	char LSHSTT[8];
	char LNHSTVS[80];
	char LDHSTVS[8];
	char LTHSTVS[8];
	char LSHSTVS[8];
	char LNHSTVO[80];
	char LDHSTVO[8];
	char LTHSTVO[8];
	char LSHSTVO[8];
	char *MCFLDS;
	
}CCMC;
typedef struct ccmr {
	double *sigapb;
	double *siga;
	double *sigb;
	double *mplat;
	double *mpwts;
}CCMR;

typedef struct _CcmDimInqRecList CcmDimInqRecList;
typedef struct _CcmAttInqRecList CcmAttInqRecList;
typedef struct _CcmVarInqRecList CcmVarInqRecList;
typedef struct _CcmIntVarInqRecList CcmIntVarInqRecList;
typedef struct _CcmHeader CcmHeader;
typedef struct _NclCcmFVarRec NclCcmFVarRec;

struct _NclCcmFVarRec {
        NclQuark var_name_quark;
        NclBasicDataTypes data_type;
        int     num_dimensions;
        ng_size_t     dim_sizes[NCL_MAX_DIMENSIONS];
        ng_size_t     file_dim_num[NCL_MAX_DIMENSIONS];
};


struct _CcmIntVarInqRecList {
	NclQuark var_name_q;
	NclCcmFVarRec var_info;
	NclMultiDValData thevalue;
	int n_atts;
	CcmAttInqRecList *theatts;
	struct _CcmIntVarInqRecList *next;
};

struct _CcmVarInqRecList {
	NclQuark var_name_q;		/*
					 * take from cheader.MCFLDS
					 */
	NclCcmFVarRec var_info;		/*
					 * vars are dimensioned <= 4
					 * if 4 then [time]x[lat]x[lev]x[lon]
					 * if 3 either [lat]x[lev]x[lon] or
					 *             [time]x[lat]x[lon] 
					 * if 2 then [lat]x[lon]
					 * if 1 either [lev] or
					 *	       [lon] or
                                         *             [lat] or
                                         *             [derived]
					 * sizeof(lat) == iheader.NOREC
					 * sizeof(lon) == iheader.NLON
					 * sizeof(lev) == iheader.NLEV
					 * sizeof(time) == iheader.MFILTH
					 */
	int 	ccm_var_index;
	int	offset;			/*
					 * offset in words from begining
					 * of latitude record.
					 */
	int 	packing; 		/* packing for each record 
 					 * iheader.MFLDS[ifield][2] 
					 */
	int	level_type; 		/* level 0==single-level, 
					 * 1=multilevel field at layer 
					 * interfaces and 2=multilevel 
					 * field at layer mid-points 
					 */
	int	sample_type;		/* 0== instantaneous
					 * 1==averaged
					 * 2== minimum for each grid-	
					 *     point since last
					 *     sample write
                                         * 3 == maximum for each grid-   
                                         *     point since last 
					 *     sample write
                                         */
};

struct _CcmHeader{
	CCMI iheader; /* integer header from CCM file */
	CCMC cheader; /* char header from CCM file */
	CCMR rheader; /* real header */
};

struct _CcmDimInqRecList {
	int dim_number;
	NclQuark dim_name;
	long size;
};
struct _CcmAttInqRecList {
	NclQuark attname;
	NclMultiDValData thevalue;
	struct _CcmAttInqRecList *next;
};

typedef struct ccm_file_rec {
	NclQuark file_path_q;
	int	wr_status;
	int 	cos_blocking;
	int 	n_lat_recs;
	long*   lat_rec_offsets;
	int	n_vars;
	int 	n_headers;		/*
					 * number of time records in file iheader.MFILTH
					 */
	CcmHeader 	header;
	CcmVarInqRecList *vars;
	int 	n_int_vars;
	CcmIntVarInqRecList *int_vars;
	int 	n_dims;
	CcmDimInqRecList *dims;
	int	n_file_atts;		 
	CcmAttInqRecList *file_atts; 	/*
					 * mostly attributes from 
					 * file headers
					 */
} CCMFileRec;


typedef struct ncl_q_list {
	NclQuark var_quark;
	int	var_q_index;
}NclQList;

#define BLOCK_SIZE 4096
#define WORD_SIZE 8
#define INT_HEADER_SIZE 37
#define CHAR_HEADER_SIZE 89
#define CEOF (char)0016
#define CEOD (char)0017
#define CEOR (char)0010
#define CBCW (char)0000

#define SCALAR_DIM_NUMBER 0
#define CHAR_DIM_NUMBER 1
#define TIME_DIM_NUMBER 2
#define LATITUDE_DIM_NUMBER 3
#define ILEV_DIM_NUMBER 4
#define MLEV_DIM_NUMBER 5
#define LONGITUDE_DIM_NUMBER 6
typedef struct {
  	unsigned int type  :  4;
  	unsigned int junk  : 27;
  	unsigned int bnhi  :  1;
  	unsigned int bnlo  : 23;
  	unsigned int fwi   :  9;
}BBCCWW;
typedef union{
	BBCCWW bc;
	char thebuf[8];
} BCW;

/*
  The BCW bitfield is used to extract the type of control word and the
  block number from a block control word.  In an 8 byte Cray control word
  the block number would be read as a single 24 bit piece of data.  In
  BCW the block number is split into two pieces, bnhi and bnlo, so that
  the block number field does not span a word boundary on a machine with
  4-byte words.

  See the Cray manpage BLOCKED(4F) for more information.
*/
