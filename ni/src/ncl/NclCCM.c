#include <stdlib.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "date.h"
#include "NclCCM.h"
#include <math.h>

NrmQuark CcmVarName
#if	NhlNeedProto
(char *buffer)
#else
(buffer)
char *buffer;
#endif
{
	char tmpc[WORD_SIZE+1];
	int i;

	i = 0;	
	while((i<WORD_SIZE)&&(buffer[i] != ' ')) {
		tmpc[i] = buffer[i];
		i++;
	}
	tmpc[i] = '\0';
	return(NrmStringToQuark(tmpc));
}

static void *CcmCreateFileRec
#if	NhlNeedProto
(NclQuark path)
#else
(path)
NclQuark path;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: NCL does not support creating files in the CCM history format, create netCDF instead");
	return(NULL);
}

static int IsCOSBlockedCCM
#if NhlNeedProto
(int fd)
#else
(fd)
int fd;
#endif
{
	return(1);
}
static int IsNONBlockedCCM
#if NhlNeedProto
(int fd)
#else
(fd)
int fd;
#endif
{
	return(1);
}
double *DoubleEm(char* buffer,int nwords) {
	int zero = 0;
	int total = nwords;
	double *tmp_out = NclMalloc(sizeof(double)*nwords);
	ctodpf(buffer,tmp_out,&total,&zero);
	return(tmp_out);
}
double DoubleIt(char* buffer) {
	int zero = 0;
	int total = 1;
	double tmp_out;
	ctodpf(buffer,&tmp_out,&total,&zero);
	return(tmp_out);
}
int *IntEm(char* buffer,int nwords) {
	int zero = 0;
	int total = nwords;
	int *tmp_out = NclMalloc(sizeof(int)*nwords);
	ctospi(buffer,tmp_out,&total,&zero);
	return(tmp_out);
}
int IntIt(char* buffer) {
	int zero = 0;
	int total = 1;
	int tmp_out;
	ctospi(buffer,&tmp_out,&total,&zero);
	return(tmp_out);
}
float *FloatEm(char* buffer,int nwords) {
	int zero = 0;
	int total = nwords;
	float *tmp_out = NclMalloc(sizeof(float)*nwords);
	ctospf(buffer,tmp_out,&total,&zero);
	return(tmp_out);
}
float FloatIt(char* buffer) {
	int zero = 0;
	int total = 1;
	float tmp_out;
	ctospf(buffer,&tmp_out,&total,&zero);
	return(tmp_out);
}
int CompareHeaders(CCMI *initial_iheader,CCMC *initial_cheader,CCMR *initial_rheader,CCMI* tmp_iheader,CCMC* tmp_cheader,CCMR* tmp_rheader) {
	return(1);
}

long MySeek
#if NhlNeedProto
(CCMFileRec *therec,int fd,int nwords,long start_off)
#else
(therec,fd,nwords,start_off)
CCMFileRec *therec;
int fd;
int nwords;
long start_off;
#endif
{
	if(nwords == 0) {
		lseek(fd,start_off,SEEK_SET);
		return(start_off);
	} else if(therec->cos_blocking) {
		return(start_off);
	} else {
		return(start_off);
	}
}

long MyRead
#if NhlNeedProto
(CCMFileRec *therec,int fd,char *buffer,int nwords, long start_off)
#else
(therec,fd,buffer,nwords, start_off)
CCMFileRec *therec;
int fd;
char *buffer;
int nwords;
long start_off;
#endif
{
        lseek(fd,start_off,SEEK_SET);
        if(nwords == 0) {
                return(start_off);
        } else if(therec->cos_blocking){
                return(start_off);
        } else {
                return(start_off);
        }
}
long sz(int n)
{
        return(WORD_SIZE * n);
}


char EndOfRec(char cw[]) {
	char tmpc;
	
	tmpc = cw[0] & 0360;
	tmpc = tmpc >> 4;
	if(tmpc == (char)010) {
		return(CEOR);
	} else if(tmpc == (char)017) {
		return(CEOD);
	} else if(tmpc == (char)016) {
		return(CEOF);
	} else if(tmpc == (char)0) {
		return(CBCW);
	} else {
		fprintf(stdout, "ERROR:");
		return((char)0377);
	}
}
int forward_index(char cw[]) {
	char buffer[WORD_SIZE];
	
	buffer[0] =0;
	buffer[1] = 0;
	buffer[2] = 0;
	buffer[3] = 0;
	buffer[4] = 0;
	buffer[5] = 0;
	buffer[6] = (char)01 & cw[6];
	buffer[7] = (char)0377 & cw[7];
	return(IntIt(buffer));
}

long COSRecSeek(int fd,int n_words, long current_offset) 
{
	long totsz = sz(n_words);
	int cb = current_offset / BLOCK_SIZE;
	int cof = current_offset % BLOCK_SIZE;
	int nb = 0;

	if((BLOCK_SIZE - cof) > totsz) {
		lseek(fd,cb * BLOCK_SIZE + cof + totsz,SEEK_SET);
		return(cb * BLOCK_SIZE + cof + totsz);
	} else {
		if(cof != 0) {
			totsz = totsz - (BLOCK_SIZE - cof);
		} 
		nb = (totsz)/(BLOCK_SIZE-sz(1));
		totsz -= nb * (BLOCK_SIZE-sz(1));
		if(totsz >= BLOCK_SIZE) {
			fprintf(stdout,"Error1\n");
		}
		lseek(fd,(cb+nb)*BLOCK_SIZE + totsz + BLOCK_SIZE + sz(1),SEEK_SET);
		return((cb+nb)*BLOCK_SIZE + totsz + BLOCK_SIZE + sz(1));
	}
}
long COSGetNWords(int fd,int num_words,long current_offset,char *buffer)
{
	long totsz = sz(num_words);
	int cb = current_offset / BLOCK_SIZE;
	int cof = current_offset % BLOCK_SIZE;
	int nb;
	int i;
	int n = 0;
	int index = 0;
	char control[WORD_SIZE];
	
	lseek(fd,current_offset,SEEK_SET);
	if(totsz < (BLOCK_SIZE - cof)) {
		n = read(fd,buffer,totsz);
		index +=n;
		return(cb * BLOCK_SIZE + cof + totsz);
	} else {
	
		totsz = totsz - (BLOCK_SIZE - cof);
		n = read (fd,buffer,(BLOCK_SIZE - cof));
		index += n;
		nb = (totsz)/(BLOCK_SIZE-sz(1));
		for(i = 0; i < nb; i++) {
			n = read(fd,control,WORD_SIZE);
			n = read (fd,&(buffer[index]),(BLOCK_SIZE - sz(1)));
			index += n;
		} 
		totsz -= nb * (BLOCK_SIZE - sz(1));
		if(totsz >= BLOCK_SIZE) {
			fprintf(stdout,"Error1\n");
		}
		n = read(fd,control,WORD_SIZE);
		n = read (fd,&(buffer[index]),totsz);
		index += n;
		return(COSRecSeek(fd,num_words,current_offset));
	}
}
int COSGetRecord(int fd, int block_number,int offset,char **buffer,int* finish_block,int* finish_offset)
{
	long real_offset = (block_number * BLOCK_SIZE) + sz(offset);
	long end_offset;
	char control_word[BLOCK_SIZE];
	int total = 0;
	int len;
	int n;
	int index = 0;
	char tmpc;

	end_offset = real_offset = lseek(fd,real_offset,SEEK_SET);
	n = read(fd,control_word,WORD_SIZE);
	if(n != WORD_SIZE) 
		return(-1);
	while(forward_index(control_word)==0) {
		end_offset += sz(1);
		n = read(fd,control_word,WORD_SIZE);
		if(n != WORD_SIZE) {
			return(-1);
		}
	}
        control_word[0] = control_word[0] & (char)0017;
	while((EndOfRec(control_word) != CEOR)){
		end_offset += sz(1);
		len = forward_index(control_word);	
		total += len;
		end_offset += sz(len);
		end_offset = lseek(fd,end_offset,SEEK_SET);
		n = read(fd,control_word,WORD_SIZE);
		if( n != WORD_SIZE) 
			return(-1);
	};
	*buffer = (char*)NclMalloc(sz(total));
	lseek(fd,real_offset,SEEK_SET);
	n = read(fd,control_word,WORD_SIZE);
	if( n != WORD_SIZE) 
		return(-1);
        control_word[0] = control_word[0] & (char)0017;
	while(EndOfRec(control_word)!=CEOR){
                len = forward_index(control_word);
		n = read(fd,&((*buffer)[index]),sz(len));
		if( n != sz(len)) 
			return(-1);
		index += sz(len);
		n =read(fd,control_word,WORD_SIZE);
		if( n != WORD_SIZE) 
			return(-1);
        }
	lseek(fd,end_offset,SEEK_SET);
	*finish_block = end_offset / BLOCK_SIZE;
	*finish_offset = (end_offset % BLOCK_SIZE)/WORD_SIZE;
	return(total);
}
int MyGetRecord
#if NhlNeedProto
(CCMFileRec *therec,int fd, int start_block ,int start_offset,char **buffer,int *finish_block,int *finish_offset)
#else
(therec,fd,start_block ,start_offset,buffer,finish_block,finish_offset)
CCMFileRec *therec;
int fd;
int start_block;
int start_offset;
char **buffer;
int *finish_block;
int *finish_offset;
#endif
{
	if(therec->cos_blocking) {
		return(COSGetRecord(fd,start_block ,start_offset,buffer,finish_block,finish_offset));
	} else {
		return(-1);
	}
	
}



long UnPackRealHeader(CCMFileRec *therec,int fd,CCMI *iheader, CCMR *header,int start_block, int start_offset)
{
	int i;
	int zero = 0;
	int total;
	int index;
	int n;
	long curr_offset = start_offset;
	char *buffer;
	int finish_block;
	int finish_offset;
	
	

	n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset);
	if(n == -1) {
		return(n);
	}


	index = 0;
	header->sigapb = DoubleEm(&(buffer[index]),iheader->NLEV*2 + 1);
	index += sz(iheader->NLEV*2 + 1);
	header->siga = DoubleEm(&(buffer[index]),iheader->NLEV*2 + 1);
	index += sz(iheader->NLEV*2 + 1);
	header->sigb = DoubleEm(&(buffer[index]),iheader->NLEV*2 + 1);
	index += sz(iheader->NLEV*2 + 1);

	header->mplat = DoubleEm(&(buffer[index]),iheader->NOREC);
	index += sz(iheader->NOREC);
	
	header->mpwts = DoubleEm(&(buffer[index]),iheader->NOREC);
	index += sz(iheader->NOREC);
	
	free(buffer);	
	return((finish_block * BLOCK_SIZE) + sz(finish_offset));

}
long UnPackCharHeader(CCMFileRec *therec,int fd,CCMI *iheader, CCMC *header,int start_block, int start_offset)
{
	int i;
	int zero = 0;
	int total;
	int index;
	int n;
	long curr_offset = start_offset;
	char *buffer;
	int finish_block;
	int finish_offset;

	n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset);
	if(n == -1) {
		return(n);
	}

	index = 0;
	memcpy(header,buffer,sz(CHAR_HEADER_SIZE));
	index += sz(CHAR_HEADER_SIZE);

	header->MCFLDS = (char*)NclMalloc(sz(2 * iheader->NFLDH));

	memcpy(header->MCFLDS,&(buffer[index]),sz(2 * iheader->NFLDH));
	index += 2 * iheader->NFLDH;

	free(buffer);	
	return((finish_block * BLOCK_SIZE) + sz(finish_offset));

}
long UnPackIntHeader(CCMFileRec *therec,int fd, CCMI *header,int start_block, int start_offset)
{
	int i;
	int zero = 0;
	int total;
	int index;
	int n;
	long curr_offset = start_offset;
	char *buffer;
	int finish_block;
	int finish_offset;

	n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset);
	if(n == -1) {
		return(n);
	}

	total = INT_HEADER_SIZE;	
	index = 0;
	ctospi(buffer,header,&total,&zero);
	index += sz(INT_HEADER_SIZE);


	total = 3 * header->NFLDH;	
	header->MFLDS = IntEm(&(buffer[index]),3 * header->NFLDH);
	index += sz(3 * header->NFLDH);
	

	free(buffer);	
	return(finish_block * BLOCK_SIZE + sz(finish_offset));
}
static void *CcmGetFileRec
#if	NhlNeedProto
(NclQuark path,int wr_status)
#else
(path,wr_status)
NclQuark path;
int	wr_status;
#endif
{
	CCMFileRec *therec = NULL;
	CCMI initial_iheader;
	CCMC initial_cheader;
	CCMR initial_rheader;
	CCMI tmp_iheader;
	CCMC tmp_cheader;
	CCMR tmp_rheader;
	int i,j,k,l;
	int dim_num;
	long coff = 0;  /* keeps track of current offset in bytes into the file */
	int cb = 0; 	/* keeps track of current block in numbers of blocks */
	int cb_off = 0; /* keeps track of word offset into block number cb  cb * BLOCK_SIZE + cb_off * WORD_SIZE == coff */
	int tmp_off;
	char buffer[BLOCK_SIZE];
	int index;
	
	int fd;
	fd = open(NrmQuarkToString(path),O_RDONLY);
	if(fd > 0) {
/*
* If its COS blocked it will set up MySeek and MyRead pointers for COS reads
* If its NON blocked cray binary file the MySeek and MyRead pointer 
*/
		therec = (CCMFileRec*)NclMalloc(sizeof(CCMFileRec));
		if(IsCOSBlockedCCM(fd)||IsNONBlockedCCM(fd)) {
			therec->cos_blocking = IsCOSBlockedCCM(fd);
			therec->file_path_q = path;
			therec->wr_status = wr_status;
			therec->n_headers = NULL;
			therec->n_vars = 0;	
			therec->vars = NULL;
			therec->n_file_atts = 0;
			therec->file_atts= NULL;
/*
* Obtain first integer header and set up info arrays
*/
			coff = UnPackIntHeader(therec,fd,&initial_iheader,cb,cb_off);
			if(coff == -1) {
				NclFree(therec);
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM integer header for file (%s)",NrmQuarkToString(path));
				return(NULL);
			}

			therec->n_dims = LONGITUDE_DIM_NUMBER + 1;
			therec->dims = (CcmDimInqRecList*)NclMalloc(therec->n_dims * sizeof(CcmDimInqRecList));

			therec->dims[TIME_DIM_NUMBER].dim_name= NrmStringToQuark("time");
			therec->dims[LATITUDE_DIM_NUMBER].dim_name= NrmStringToQuark("latitude");
			therec->dims[ILEV_DIM_NUMBER].dim_name= NrmStringToQuark("ilev");
			therec->dims[MLEV_DIM_NUMBER].dim_name= NrmStringToQuark("mlev");
			therec->dims[LONGITUDE_DIM_NUMBER].dim_name= NrmStringToQuark("longitude");
			therec->dims[TIME_DIM_NUMBER].size = initial_iheader.MFILTH;
			therec->dims[LATITUDE_DIM_NUMBER].size = initial_iheader.NOREC;
			therec->dims[ILEV_DIM_NUMBER].size = initial_iheader.NLEV;
			therec->dims[MLEV_DIM_NUMBER].size = initial_iheader.NLEV;
			therec->dims[LONGITUDE_DIM_NUMBER].size = initial_iheader.NLON;


			cb = coff / BLOCK_SIZE;
			cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
			coff = UnPackCharHeader(therec,fd,&initial_iheader,&initial_cheader,cb,cb_off);
			if(coff == -1) {
				NclFree(therec);
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM character header for file (%s)",NrmQuarkToString(path));
				return(NULL);
			}
			cb = coff / BLOCK_SIZE;
			cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
			coff = UnPackRealHeader(therec,fd,&initial_iheader,&initial_rheader,cb,cb_off);
			if(coff == -1) {
				NclFree(therec);
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM real header for file (%s)",NrmQuarkToString(path));
				return(NULL);
			}
/*
* coff is NOW POINTING!!! to the top of the first latitude record!!!!
*/
			therec->n_vars = initial_iheader.NFLDH;
			therec->n_headers = initial_iheader.MFILTH;
			therec->lat_rec_offsets = (long*)NclMalloc(sizeof(long) * initial_iheader.NOREC);
			therec->n_lat_recs = initial_iheader.MFILTH*initial_iheader.NOREC;
			therec->vars = (CcmVarInqRecList*)NclMalloc(sizeof(CcmVarInqRecList)*therec->n_vars);
			for(i = 0; i < therec->n_vars; i ++) {
				 
				therec->vars[i].offset = initial_iheader.MFLDS[i*3+ 1];
				therec->vars[i].packing = initial_iheader.MFLDS[i*3 + 2];
				therec->vars[i].level_type = initial_iheader.MFLDS[i*3] % 10;
				therec->vars[i].sample_type = initial_iheader.MFLDS[i*3] / 10;
				therec->vars[i].n_atts = 0;	
				therec->vars[i].theatts = NULL;

				therec->vars[i].var_name_q = therec->vars[i].var_info.var_name_quark = CcmVarName(&(initial_cheader.MCFLDS[2*WORD_SIZE * i]));
				if(therec->vars[i].packing  > 1) {
					therec->vars[i].var_info.data_type = NCL_float;
				} else {
					therec->vars[i].var_info.data_type = NCL_double;
				}
				dim_num = 0;
				if(therec->n_headers > 1) {
					therec->vars[i].var_info.dim_sizes[dim_num] = therec->n_headers;
					therec->vars[i].var_info.file_dim_num[dim_num] = TIME_DIM_NUMBER;
					dim_num++;
				}
				if(initial_iheader.NOREC > 1) {
					therec->vars[i].var_info.dim_sizes[dim_num] = initial_iheader.NOREC;
					therec->vars[i].var_info.file_dim_num[dim_num] = LATITUDE_DIM_NUMBER;
					dim_num++;
				}
				switch(therec->vars[i].level_type) {
				case 0:
					break;
				case 1:
					therec->vars[i].var_info.dim_sizes[dim_num] = initial_iheader.NLEV;
					therec->vars[i].var_info.file_dim_num[dim_num] = ILEV_DIM_NUMBER;
					dim_num++;
					break;
				case 2:	
					therec->vars[i].var_info.dim_sizes[dim_num] = initial_iheader.NLEV;
					therec->vars[i].var_info.file_dim_num[dim_num] = MLEV_DIM_NUMBER;
					dim_num++;
					break;
				default:
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: An incorrect value was detected suggesting an error in the CCM file (%s)",NrmQuarkToString(path));
					NclFree(therec);
					return(NULL);
				}
				therec->vars[i].var_info.dim_sizes[dim_num] = initial_iheader.NLON;
				therec->vars[i].var_info.file_dim_num[dim_num] = LONGITUDE_DIM_NUMBER;
				dim_num++;
				therec->vars[i].var_info.num_dimensions = dim_num;
			}
			j = 1;
			for(i = 0; i< tmp_iheader.MFILTH;i++) {
				for( j = 0; j < tmp_iheader.NOREC; j++) {
					coff = MySeek(therec,fd,tmp_iheader.MAXSIZ,coff);
					tmp_off = MyRead(therec,fd,buffer,1,coff);
					index = (int)FloatIt(buffer);
					therec->lat_rec_offsets[(tmp_iheader.MFILH - 1)*tmp_iheader.NOREC+(index-1)] = coff;
				}
/*
* coff NOW POINTING TO BEGINING OF NEXT TIME STEP, have to unpack or skip headers
* Need to actual compare verses pervious header so errors can be detected but for now this is ok
* note that tmp_iheader.MFILH is used as time_index just in case time isn't written
* linearly into history files similar to lat records.
*/
				coff = UnPackIntHeader(therec,fd,&tmp_iheader,cb,cb_off);
				if(coff == -1) {
					NclFree(therec);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM integer header for file (%s)",NrmQuarkToString(path));
					return(NULL);
				}
				cb = coff / BLOCK_SIZE;
				cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
				coff = UnPackCharHeader(therec,fd,&tmp_iheader,&tmp_cheader,cb,cb_off);
				if(coff == -1) {
					NclFree(therec);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM character header for file (%s)",NrmQuarkToString(path));
					return(NULL);
				}
				cb = coff / BLOCK_SIZE;
				cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
				coff = UnPackRealHeader(therec,fd,&tmp_iheader,&tmp_rheader,cb,cb_off);
				if(coff == -1) {
					NclFree(therec);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM real header for file (%s)",NrmQuarkToString(path));
					return(NULL);
				}
				if(!CompareHeaders(&initial_iheader,&initial_cheader,&initial_rheader,
						    &tmp_iheader,&tmp_cheader,&tmp_rheader)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Comparision of headers failed, headers for timestep number (%d) vary unacceptably from initial header, NCL doesn't handle this",i+1);
					NclFree(therec);
					return(NULL);

				}
			}
			return(therec);
		} else {
                	NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: Could not open (%s), was not a recognized as a CCM format file",NrmQuarkToString(path));
		}
	} else if(fd <= 0) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: Could not open (%s) check permissions",NrmQuarkToString(path));
        }
        return(NULL);
}

static void CcmFreeFileRec
#if	NhlNeedProto
(void * therec) 
#else
(therec)
void *therec;
#endif
{
	return;
}

static NclQuark *CcmGetVarNames
#if	NhlNeedProto
(void * therec, int *n_vars)
#else
(therec, n_vars)
void * therec;
int *n_vars;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclQuark *arout;
	int i;
	*n_vars = thefile->n_vars;

	arout = NclMalloc(sizeof(NclQuark)* *n_vars);
	for(i = 0; i < *n_vars; i++ ) {
		arout[i] = thefile->vars[i].var_info.var_name_quark;
	}	
	return(arout);
}

static NclFVarRec* CcmGetVarInfo
#if     NhlNeedProto
(void * therec, NclQuark var_name)
#else
(therec, n_vars)
void * therec;
NclQuark var_name;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclFVarRec *tmp = NULL;
	int i;
	for(i = 0; i < thefile->n_vars; i++ ) {

		if(var_name == thefile->vars[i].var_name_q) {
			tmp = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec));
			*tmp = thefile->vars[i].var_info;
		}
	}
	
        return(tmp);
}

static NclQuark* CcmGetDimNames
#if     NhlNeedProto
(void * therec, int * n_dims) 
#else
(therec, n_dims) 
void * therec;
int * n_dims;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclQuark *dims;
	int i;

	*n_dims = thefile->n_dims;
	dims = (NclQuark*)NclMalloc(sizeof(NclQuark)*thefile->n_dims);
	for(i = 0; i < thefile->n_dims; i++) {
		dims[i] = thefile->dims[i].dim_name;
	}
	return(dims);
}

static NclFDimRec * CcmGetDimInfo
#if	NhlNeedProto
(void * therec, NclQuark dim_name)
#else
(therec, dim_name)
void * therec;
NclQuark dim_name;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclFDimRec *dims = NULL;
	int i;
	for(i = 0; i < thefile->n_dims; i++) {
		if(thefile->dims[i].dim_name == dim_name) {
			dims = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			dims->dim_name_quark = thefile->dims[i].dim_name;
			dims->dim_size = thefile->dims[i].size;
		}
	}
	return(dims);
}

static NclQuark *CcmGetAttNames
#if NhlNeedProto
(void *therec, int *n_atts)
#else
(therec, n_atts)
void *therec;
int *n_atts;
#endif
{
	*n_atts = 0;
	return(NULL);
}

static NclFAttRec * CcmGetAttInfo
#if NhlNeedProto
(void * therec, NclQuark attname)
#else
(therec,attname)
void *therec;
NclQuark attname;
#endif
{
	return(NULL);
}

static NclQuark *CcmGetVarAttNames
#if NhlNeedProto
(void * therec, NclQuark var_name, int *n_atts)
#else
(therec, var_name, n_atts)
void * therec;
NclQuark var_name;
int *n_atts;
#endif
{
	*n_atts = 0;
	return(NULL);
}

static NclFAttRec *CcmGetVarAttInfo
#if     NhlNeedProto
(void *therec, NclQuark var_name, NclQuark att_name) 
#else 
(therec, var_name, att_name) 
void *therec;
NclQuark var_name;
NclQuark att_name;
#endif
{
	return(NULL);
}

static NclFVarRec *CcmGetCoordInfo
#if	NhlNeedProto
(void *therec, NclQuark dim_name)
#else 
(therec, dim_name)
void *therec;
NclQuark dim_name;
#endif
{
	return(NULL);
}

static void *CcmReadCoord
#if	NhlNeedProto
(void * therec, NclQuark dim_name,long *start,long *finish, long* stride, void *storage)
#else
(therec, dim_name,start,finish, stride, storage)
void * therec;
NclQuark dim_name;
long *start;
long *finish;
long* stride;
void *storage;
#endif
{
	return(NULL);
}

static void *CcmReadVar
#if	NhlNeedProto
(void *therec, NclQuark var_name,long *start,long* finish, long * stride, void *storage)
#else
(therec, var_name,start,finish, stride, storage)
void *therec;
NclQuark var_name;
long *start;
long* finish
long * stride
void *storage
#endif
{
	return(NULL);
}

static void *CcmReadAtt
#if	NhlNeedProto
(void *therec, NclQuark att_name, void *storage)
#else
(therec, att_name, storage)
void *therec;
NclQuark att_name;
void *storage;
#endif
{
	return(NULL);
}

static void *CcmReadVarAtt
#if	NhlNeedProto
(void *therec, NclQuark var_name,NclQuark att_name, void *storage)
#else
(therec, var_name, att_name, storage)
void *therec;
NclQuark var_name;
NclQuark att_name;
void *storage;
#endif
{
	return(NULL);
}

static NclBasicDataTypes CcmMapToNcl
#if     NhlNeedProto
( void * type)
#else
(type)
void *type;
#endif
{
	return(NCL_obj);
}

static void* CcmMapFromNcl 
#if     NhlNeedProto
(NclBasicDataTypes type)
#else
(type)
NclBasicDataTypes type;
#endif
{
	return(NULL);
}




NclFormatFunctionRec CCMRec = {
/* NclCreateFileRecFunc	   create_file_rec; */		CcmCreateFileRec,
/* NclGetFileRecFunc       get_file_rec; */		CcmGetFileRec,
/* NclFreeFileRecFunc      free_file_rec; */		CcmFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		CcmGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		CcmGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		CcmGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		CcmGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		CcmGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		CcmGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	CcmGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		CcmGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		CcmGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		CcmReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			CcmReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			CcmReadAtt,
/* NclReadVarAttFunc       read_var_att; */		CcmReadVarAtt,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		NULL,
/* NclWriteVarAttFunc      write_var_att; */		NULL,
/* NclAddDimFunc           add_dim; */			NULL,
/* NclAddDimFunc           rename_dim; */		NULL,
/* NclAddVarFunc           add_var; */			NULL,
/* NclAddVarFunc           add_coord_var; */		NULL,
/* NclAddAttFunc           add_att; */			NULL,
/* NclAddVarAttFunc        add_var_att; */		NULL,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	CcmMapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	CcmMapFromNcl,
/* NclDelAttFunc           del_att; */			NULL,
/* NclDelVarAttFunc        del_var_att; */		NULL
};

NclFormatFunctionRecPtr CcmAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	
	return(&CCMRec);
}
