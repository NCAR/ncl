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
#include "ccmhdr.h"
long sz(int n)
{
        return(WORD_SIZE * n);
}
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
	BCW cw;
	char bytes[8][511];

	read(fd,&cw,sizeof(BCW));
	
	if ( cw.type != 0 || cw.bnhi != 0 || cw.bnlo != 0 ) return 0;

	 /* Skip over the rest of the first block. */

  	if ( read(fd, bytes, WORD_SIZE*511) != 511*WORD_SIZE ) return 0;

  	/* Read and interpret second control word. */

  	if ( read(fd, &cw, sizeof( BCW )) != sizeof(BCW) ) return 0;

  	if ( cw.type != 0 || cw.bnhi != 0 || cw.bnlo != 1 ) return 0;

	lseek(fd,0,SEEK_SET);

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
		return(COSRecSeek(fd,nwords, start_off));
	} else {
		lseek(fd,start_off+sz(nwords-1),SEEK_SET);
		return(start_off + sz(nwords-1));
	}
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
		return(COSGetNWords(fd,nwords,start_off,buffer));
        } else {
		lseek(fd,start_off,SEEK_SET);
                return(read(fd,buffer,nwords*WORD_SIZE));
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
(CCMFileRec *therec,int fd, int start_block ,int start_offset,char **buffer,int *finish_block,int *finish_offset,int* rec_size)
#else
(therec,fd,start_block ,start_offset,buffer,finish_block,finish_offset,rec_size)
CCMFileRec *therec;
int fd;
int start_block;
int start_offset;
char **buffer;
int *finish_block;
int *finish_offset;
int *rec_size;
#endif
{
	long end_offset;
	if(therec->cos_blocking) {
		return(COSGetRecord(fd,start_block ,start_offset,buffer,finish_block,finish_offset));
	} else {
		if(rec_size != NULL) {
			*buffer = NclMalloc(*rec_size * WORD_SIZE);
			end_offset = start_block * BLOCK_SIZE + start_offset * WORD_SIZE + *rec_size * WORD_SIZE;
			*finish_block = end_offset / BLOCK_SIZE;
			*finish_offset = (end_offset % BLOCK_SIZE) / WORD_SIZE;
			lseek(fd,start_block * BLOCK_SIZE + start_offset * WORD_SIZE, SEEK_SET);
			return(read(fd,*buffer,*rec_size * WORD_SIZE));
		} else {
			return(-1);
		}
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
	
	

	n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset,&(iheader->LENHDR));
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

	n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset,&(iheader->LENHDC));
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
	int len;
	char cw[WORD_SIZE];

	if(!therec->cos_blocking) {
		n = MyRead(therec,fd,cw,1, start_block * BLOCK_SIZE + sz(start_offset));
		total = 1;
		ctospi(cw,&len,&total,&zero);
		n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset,&len);
	} else {
		n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset,NULL);
	}
	
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

static int qsort_compare_func
#if     NhlNeedProto
(Const void* s1,Const void* s2)
#else
(s1,s2)
void* s1;
void* s2;
#endif
{
        logical res;
        NclQList *ind1 = (NclQList*)s1;
        NclQList *ind2 = (NclQList*)s2;

	return((int)(ind1->var_quark - ind2->var_quark));
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
	static int first = 1;
	static NclQList *qlist = NULL;
	static int start_quark = 0;
	static int n_quarks = 0;
	int fd;
	float spacing;
	float *tmp_lon;
	int dimsize = 0;

	if(first) {
		n_quarks = sizeof(ccm_name_tab)/sizeof(CCMNAMES);
		qlist = (NclQList*)NclMalloc(sizeof(NclQList)*n_quarks);
		i = 0;
		while(ccm_name_tab[i].std_name != NULL) {
			qlist[i].var_quark = NrmStringToQuark(ccm_name_tab[i].std_name);
			qlist[i].var_q_index = i;
			i++;
		}	
		n_quarks = i;
		qsort(qlist,n_quarks,sizeof(NclQList),qsort_compare_func);
/*
		for(i = 0; i < n_quarks; i++) {
			fprintf(stdout,"%d\n",qlist[i].var_quark);
		}
*/
		first = 0;
	}
	fd = open(NrmQuarkToString(path),O_RDONLY);
	if(fd > 0) {
/*
* If its COS blocked it will set up MySeek and MyRead pointers for COS reads
* If its NON blocked cray binary file the MySeek and MyRead pointer 
*/
		therec = (CCMFileRec*)NclMalloc(sizeof(CCMFileRec));
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
		therec->header.iheader = initial_iheader;
		if(coff == -1) {
			NclFree(therec);
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM integer header for file (%s)",NrmQuarkToString(path));
					close(fd);
			return(NULL);
		}

		therec->n_dims = LONGITUDE_DIM_NUMBER + 1;
		therec->dims = (CcmDimInqRecList*)NclMalloc(therec->n_dims * sizeof(CcmDimInqRecList));

		therec->dims[TIME_DIM_NUMBER].dim_name= NrmStringToQuark("time");
		therec->dims[LATITUDE_DIM_NUMBER].dim_name= NrmStringToQuark("latitude");
		therec->dims[ILEV_DIM_NUMBER].dim_name= NrmStringToQuark("ilev");
		therec->dims[MLEV_DIM_NUMBER].dim_name= NrmStringToQuark("lev");
		therec->dims[LONGITUDE_DIM_NUMBER].dim_name= NrmStringToQuark("longitude");
		therec->dims[TIME_DIM_NUMBER].size = initial_iheader.MFILTH;
		therec->dims[LATITUDE_DIM_NUMBER].size = initial_iheader.NOREC;
		therec->dims[ILEV_DIM_NUMBER].size = initial_iheader.NLEV + 1;
		therec->dims[MLEV_DIM_NUMBER].size = initial_iheader.NLEV;
		therec->dims[LONGITUDE_DIM_NUMBER].size = initial_iheader.NLON;


		cb = coff / BLOCK_SIZE;
		cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
		coff = UnPackCharHeader(therec,fd,&initial_iheader,&initial_cheader,cb,cb_off);
		therec->header.cheader = initial_cheader;
		if(coff == -1) {
			NclFree(therec);
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM character header for file (%s)",NrmQuarkToString(path));
					close(fd);
			return(NULL);
		}
		cb = coff / BLOCK_SIZE;
		cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
		coff = UnPackRealHeader(therec,fd,&initial_iheader,&initial_rheader,cb,cb_off);
		therec->header.rheader = initial_rheader;
		if(coff == -1) {
			NclFree(therec);
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM real header for file (%s)",NrmQuarkToString(path));
					close(fd);
			return(NULL);
		}
		therec->coords = NclMalloc(sizeof(NclMultiDValData)*5);
		therec->coords[TIME_DIM_NUMBER] = NULL;
		tmp_lon = (float*)NclMalloc(sizeof(float)*(initial_iheader.NLEV+1));
		fprintf(stdout,"ilev:\n");
		for(i = 0; i < initial_iheader.NLEV+1; i++) {
			tmp_lon[i] = 1000.0 * ( *(initial_rheader.siga + 2*i) + *(initial_rheader.sigb + 2*i));
			fprintf(stdout,"%f\n",tmp_lon[i]);
		}
		dimsize = therec->dims[ILEV_DIM_NUMBER].size;
		therec->coords[ILEV_DIM_NUMBER] = _NclCreateMultiDVal(
							NULL,	
							NULL,	
							Ncl_MultiDValData,
							0,
							tmp_lon,
							NULL,
							1,
							&dimsize,
							PERMANENT,
							NULL,
							_NclNameToTypeClass(NrmStringToQuark("float")));

		tmp_lon = (float*)NclMalloc(sizeof(float)*initial_iheader.NLEV);
		fprintf(stdout,"lev:\n");
		for(i = 0; i < initial_iheader.NLEV; i++) {
			tmp_lon[i] = 1000.0 * ( *(initial_rheader.siga + 2*i+1) + *(initial_rheader.sigb + 2*i+1));
			fprintf(stdout,"%f\n",tmp_lon[i]);
		}
		dimsize = therec->dims[MLEV_DIM_NUMBER].size;
		therec->coords[MLEV_DIM_NUMBER] = _NclCreateMultiDVal(
							NULL,	
							NULL,	
							Ncl_MultiDValData,
							0,
							tmp_lon,
							NULL,
							1,
							&dimsize,
							PERMANENT,
							NULL,
							_NclNameToTypeClass(NrmStringToQuark("float")));



		tmp_lon = (float*)NclMalloc(sizeof(float)*initial_iheader.NLON);
		spacing = 360.0/(initial_iheader.NLON);
		for(i = 0; i < initial_iheader.NLON; i++) {
			tmp_lon[i] = i * spacing;
		}
		therec->coords[LONGITUDE_DIM_NUMBER] = _NclCreateMultiDVal(
							NULL,
							NULL,
							Ncl_MultiDValData,
							0,
							tmp_lon,
							NULL,
							1,
							&initial_iheader.NLON,
							PERMANENT,
							NULL,
							_NclNameToTypeClass(NrmStringToQuark("float")));
		tmp_lon = (float*)NclMalloc(sizeof(float)*initial_iheader.NOREC);
		for(i = 0; i < initial_iheader.NOREC; i++) {
			tmp_lon[i] = (float)initial_rheader.mplat[i];
		}
		therec->coords[LATITUDE_DIM_NUMBER] = _NclCreateMultiDVal(
							NULL,
							NULL,
							Ncl_MultiDValData,
							0,
							tmp_lon,
							NULL,
							1,
							&initial_iheader.NOREC,
							PERMANENT,
							NULL,
							_NclNameToTypeClass(NrmStringToQuark("float")));

/*
* coff is NOW POINTING!!! to the top of the first latitude record!!!!
*/
		therec->n_vars = initial_iheader.NFLDH;
		therec->n_headers = initial_iheader.MFILTH;
		therec->n_lat_recs = initial_iheader.MFILTH*initial_iheader.NOREC;
		therec->lat_rec_offsets = (long*)NclMalloc(therec->n_lat_recs*sizeof(long));
		for(i = 0; i < therec->n_lat_recs; i++) {
			therec->lat_rec_offsets[i] = -1;
		}
		therec->vars = (CcmVarInqRecList*)NclMalloc(sizeof(CcmVarInqRecList)*therec->n_vars);
		for(i = 0; i < therec->n_vars; i ++) {
			 
			therec->vars[i].offset = initial_iheader.MFLDS[i*3+ 1];
			therec->vars[i].packing = initial_iheader.MFLDS[i*3 + 2];
			therec->vars[i].level_type = initial_iheader.MFLDS[i*3] % 10;
			therec->vars[i].sample_type = initial_iheader.MFLDS[i*3] / 10;
			therec->vars[i].n_atts = 0;	
			therec->vars[i].theatts = NULL;

			therec->vars[i].var_name_q = therec->vars[i].var_info.var_name_quark = CcmVarName(&(initial_cheader.MCFLDS[2*WORD_SIZE * i]));
			therec->vars[i].ccm_var_index = -1;
			for(j = 0; j < n_quarks; j++) {
				if(therec->vars[i].var_name_q == qlist[j].var_quark) {
					therec->vars[i].ccm_var_index = qlist[j].var_q_index;
					break;
				}
			}
			

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
					close(fd);
				return(NULL);
			}
			therec->vars[i].var_info.dim_sizes[dim_num] = initial_iheader.NLON;
			therec->vars[i].var_info.file_dim_num[dim_num] = LONGITUDE_DIM_NUMBER;
			dim_num++;
			therec->vars[i].var_info.num_dimensions = dim_num;
		}
		cb = coff / BLOCK_SIZE;
		cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
/*
		if(therec->cos_blocking) {
*/
			tmp_off = MySeek(therec,fd,1,coff);
			
			tmp_off = MyRead(therec,fd,buffer,1,tmp_off);
			index = (int)FloatIt(buffer);
			therec->lat_rec_offsets[(index-1)] = coff;
			coff = MySeek(therec,fd,initial_iheader.MAXSIZ + 1,coff);
/*
		} else {
			tmp_off = MySeek(therec,fd,0,coff);
			tmp_off = MyRead(therec,fd,buffer,1,tmp_off);
			index = (int)FloatIt(buffer);
			therec->lat_rec_offsets[(index-1)] = coff;
			coff = MySeek(therec,fd,initial_iheader.MAXSIZ,coff);
		}
*/

		j = 1;
		for(i = 0; i< initial_iheader.MFILTH;i++) {
/*
* coff NOW POINTING TO BEGINING OF NEXT TIME STEP, have to unpack or skip headers
* Need to actual compare verses pervious header so errors can be detected but for now this is ok
* note that tmp_iheader.MFILH is used as time_index just in case time isn't written
* linearly into history files similar to lat records.
*/
			for( ; j < initial_iheader.NOREC; j++) {
/*
				if(therec->cos_blocking) {
*/
					tmp_off = MySeek(therec,fd,1,coff);
					tmp_off = MyRead(therec,fd,buffer,1,tmp_off);
					index = (int)FloatIt((buffer));
					therec->lat_rec_offsets[i*initial_iheader.NOREC+(index-1)] = coff;
					coff = MySeek(therec,fd,initial_iheader.MAXSIZ + 1,coff);
/*
				} else {
					tmp_off = MySeek(therec,fd,0,coff);
					tmp_off = MyRead(therec,fd,buffer,1,tmp_off);
					index = (int)FloatIt((buffer));
					therec->lat_rec_offsets[i*initial_iheader.NOREC+(index-1)] = coff;
					coff = MySeek(therec,fd,initial_iheader.MAXSIZ,coff);
				}
*/
			}
			if( i != initial_iheader.MFILTH-1) {
				cb = coff / BLOCK_SIZE;
				cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
				tmp_off = UnPackIntHeader(therec,fd,&tmp_iheader,cb,cb_off);
				if(tmp_off == -1) {
					NclFree(therec);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM integer header for file (%s)",NrmQuarkToString(path));
					close(fd);
					return(NULL);
				}
				cb = tmp_off / BLOCK_SIZE;
				cb_off = (tmp_off % BLOCK_SIZE) / WORD_SIZE;
				tmp_off= UnPackCharHeader(therec,fd,&tmp_iheader,&tmp_cheader,cb,cb_off);
				if(tmp_off == -1) {
					NclFree(therec);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM character header for file (%s)",NrmQuarkToString(path));
					close(fd);
					return(NULL);
				}
				cb = tmp_off / BLOCK_SIZE;
				cb_off = (tmp_off % BLOCK_SIZE) / WORD_SIZE;
				tmp_off = UnPackRealHeader(therec,fd,&tmp_iheader,&tmp_rheader,cb,cb_off);
				if(tmp_off == -1) {
					NclFree(therec);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM real header for file (%s)",NrmQuarkToString(path));
					close(fd);
					return(NULL);
				}
				if(!CompareHeaders(&initial_iheader,&initial_cheader,&initial_rheader,
						    &tmp_iheader,&tmp_cheader,&tmp_rheader)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Comparision of headers failed, headers for timestep number (%d) vary unacceptably from initial header, NCL doesn't handle this",i+1);
					NclFree(therec);
					close(fd);
					return(NULL);

				} else {
					coff = tmp_off;
				}
				j = 0;
			}
		}
/*
		for(i = 0; i < initial_iheader.MFILTH * initial_iheader.NOREC; i++) {
			fprintf(stdout,"%ld\n",therec->lat_rec_offsets[i]);
		}
*/
		close(fd);
		return(therec);
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
	*n_vars = thefile->n_vars + 5;

	i = 0;

	arout = NclMalloc(sizeof(NclQuark)* *n_vars);
	arout[i++] = thefile->dims[TIME_DIM_NUMBER].dim_name;
	arout[i++] = thefile->dims[LATITUDE_DIM_NUMBER].dim_name;
	arout[i++] = thefile->dims[ILEV_DIM_NUMBER].dim_name;
	arout[i++] = thefile->dims[MLEV_DIM_NUMBER].dim_name;
	arout[i++] = thefile->dims[LONGITUDE_DIM_NUMBER].dim_name;
	for( ; i < *n_vars; i++ ) {
		arout[i] = thefile->vars[i-5].var_info.var_name_quark;
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
	for(i = 0; i < 5; i++ ) {
		if(var_name == thefile->dims[i].dim_name) {
			tmp = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec));
			tmp->var_name_quark = var_name;
			tmp->data_type = NCL_float;
			tmp->num_dimensions = 1;
			tmp->dim_sizes[0] = thefile->dims[i].size;
			tmp->file_dim_num[0] = i;
			return(tmp);
		}
	}
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
	CCMFileRec *thefile = (CCMFileRec*)therec;
	int i;
	NclQuark *arout;
	*n_atts = 0;

	arout = (NclQuark*)NclMalloc(sizeof(NclQuark)*3);
	for(i = 0; i < thefile->n_vars; i++ ) {

		if(var_name == thefile->vars[i].var_name_q) {
			if(thefile->vars[i].ccm_var_index != -1) {
				*n_atts = 3;
				arout[0] = NrmStringToQuark("long_name");
				arout[1] = NrmStringToQuark("units");
				arout[2] = NrmStringToQuark("t_op");
        			return(arout);
			} else {
				*n_atts = 2;
				arout[0] = NrmStringToQuark("units");
				arout[1] = NrmStringToQuark("t_op");
        			return(arout);
			}
			
		}
	}
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
	NclFAttRec *tmp = NclMalloc(sizeof(NclFAttRec));

	tmp->att_name_quark = att_name;
	tmp->data_type = NCL_string;
	tmp->num_elements = 1;
	return(tmp);
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
	return(CcmGetVarInfo(therec,dim_name));
}


static int MyUnPack
#if	NhlNeedProto
(CCMFileRec* therec,int fd,void *rbuffer, void* buffer, int poff, long coff, int packing,int *dimsizes )
#else
(therec, fd, rbuffer, buffer,poff, coff, packing,dimsizes)
CCMFileRec* therec;
int fd;
void *rbuffer;
void* buffer;
int poff;
long coff;
int packing;
int n_elem;
int *dimsizes;
#endif
{
	long tmp_off;
	int zero = 0;
	int total = 0;
	int n_elem= 0;
	double ll[2];
	unsigned int uval;
	unsigned short sval;
	int k,i,j;

	n_elem = dimsizes[0]*dimsizes[1];

	
	tmp_off = MySeek(therec,fd,poff,coff);
	switch(packing) {
	case 1:
		tmp_off = MyRead(therec,fd,buffer,n_elem,tmp_off);
		ctodpf(buffer,rbuffer,&total,&zero);
		return(tmp_off);
		break;
	case 2:
		tmp_off = MyRead(therec,fd,buffer,(n_elem/2) + 2*dimsizes[0],tmp_off);
		k = 0;
		for(j = 0; j < dimsizes[0]; j++) {
			total = 2;
			ctodpf(&(((char*)buffer)[k*4]),ll,&total,&zero);
			k += 4;
			for(i = 0; i < dimsizes[1]; i++) {
				memcpy(&uval,&(((char*)buffer)[k*4]),4);
				((float*)rbuffer)[j*dimsizes[1] + i] = (float)(ll[0] + ((double)uval)/ll[1]);
				k++;
			}
		}
		return(tmp_off);
		break;
	case 4:
		tmp_off = MyRead(therec,fd,buffer,n_elem/4+2,tmp_off);
		total = 2;
		ctodpf(buffer,ll,&total,&zero);
		k = 8;
		for(i = 0; i < n_elem; i++) {
			memcpy(&sval,&(((char*)buffer)[k*2]),2);
			((float*)rbuffer)[i] = (float)(ll[0] + ((double)sval)/ll[1]);
			k++;
		}
		return(tmp_off);
		break;
	default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Packing for this CCM file is not 1,2, or 4. This is an severe error can't continue");
		return(-1);
		return;
	}
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
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclSelectionRecord  sel_ptr;
	CcmVarInqRecList *tmp_var = NULL;
	int i,j,k;
	int buf_size,ns;
	char *buffer = NULL;
	char *rbuffer = NULL;
	long coff,tmp_off;
	int lstart,tstart;
	int lfinish,tfinish;
	int lstride,tstride;
	int nl,nt,fd;
	int to = 0;
	NclMultiDValData tmp_md;
	NclMultiDValData tmp_md2;
	int dimsizes[4];

	for (i = 0; i < thefile->n_vars; i++) {
		if(thefile->vars[i].var_info.var_name_quark == var_name) {
			tmp_var = &(thefile->vars[i]);	
			break;	
		}
	}
	if(i == thefile->n_vars) {
		for(i = 0; i < thefile->n_dims; i++) {
			if(var_name == thefile->dims[i].dim_name) {
				sel_ptr.n_entries = 1;
				sel_ptr.selection[0].sel_type = Ncl_SUBSCR;
                        	sel_ptr.selection[0].dim_num = 0;
                        	sel_ptr.selection[0].u.sub.start = start[0];
                        	sel_ptr.selection[0].u.sub.finish = finish[0];
                        	sel_ptr.selection[0].u.sub.stride = stride[0];
				switch(i) {
					case TIME_DIM_NUMBER:
						NhlPError(NhlFATAL,NhlEUNKNOWN,"CCMReadVar: Coordinate Variable (%s) not available",NrmQuarkToString(var_name));
						return(NULL);
						break;
					case LONGITUDE_DIM_NUMBER:
					case LATITUDE_DIM_NUMBER:
					case ILEV_DIM_NUMBER:
					case MLEV_DIM_NUMBER:
						if(thefile->coords[i] != NULL) {
							tmp_md2 = (NclMultiDValData)_NclReadSubSection((NclData)thefile->coords[i],&sel_ptr,NULL);
							memcpy(storage,tmp_md2->multidval.val,tmp_md2->multidval.totalsize);
							_NclDestroyObj((NclObj)tmp_md2);
						}
						return(storage);
						break;
				}
			}
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,"CCMReadVar: Variable (%s) undefined",NrmQuarkToString(var_name));
		return(NULL);
	} else {
		fd = open(NrmQuarkToString(thefile->file_path_q),O_RDONLY);
		k = 0;
		if(tmp_var->var_info.file_dim_num[0] == TIME_DIM_NUMBER) {
			tstart = start[k];
			tfinish = finish[k];	
			tstride = stride[k];
			k++;
			lstart = start[k];
			lfinish = finish[k];
			lstride = stride[k];
			k++;
		} else { 
			tstart = 0;
			tfinish = 0;
			tstride = 1;
			lstart = start[k];
			lfinish = finish[k];
			lstride = stride[k];
			k++;
		}
		nt = ((tfinish-tstart)/ tstride) +1;
		nl = ((lfinish-lstart)/ lstride) +1;
		ns = 1;
		buf_size = 1;
		i = 0;
		while(k < tmp_var->var_info.num_dimensions) {
			sel_ptr.selection[i].sel_type = Ncl_SUBSCR;
                        sel_ptr.selection[i].dim_num = i;
                        sel_ptr.selection[i].u.sub.start = start[k];
                        sel_ptr.selection[i].u.sub.finish = finish[k];
                        sel_ptr.selection[i].u.sub.stride = stride[k];
			dimsizes[i] = tmp_var->var_info.dim_sizes[k];
			ns *= ((finish[k] - start[k]) / stride[k]) + 1;
			buf_size *= tmp_var->var_info.dim_sizes[k];
			k++;i++;
		}
		sel_ptr.n_entries = i;
		buffer = (char*)NclMalloc((WORD_SIZE*buf_size/tmp_var->packing)+2*WORD_SIZE*(thefile->header.iheader.NLEV +1));
		rbuffer = (char*)NclMalloc(buf_size*sizeof(double));
		tmp_md = _NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			rbuffer,
			NULL,
			i,
			dimsizes,
			PERMANENT,
			NULL,
			_NclNameToTypeClass((tmp_var->packing != 1) ? NrmStringToQuark("float"):NrmStringToQuark("double")));
		for(i = 0; i < nt; i++) {
			for(j = 0; j < nl; j++)  {
				coff = thefile->lat_rec_offsets[(tstart + i * tstride) * thefile->header.iheader.NOREC + (lstart + j * lstride)];
				if(tmp_md->multidval.n_dims == 1) {
					dimsizes[0] = 1;
					dimsizes[1] = tmp_md->multidval.dim_sizes[0];
				} else {
					dimsizes[0] = tmp_md->multidval.dim_sizes[0];
					dimsizes[1] = tmp_md->multidval.dim_sizes[1];
				}
				MyUnPack(thefile,fd,rbuffer,buffer,tmp_var->offset,coff,tmp_var->packing,dimsizes);
				tmp_md2 = (NclMultiDValData)_NclReadSubSection((NclData)tmp_md,&sel_ptr,NULL);
				memcpy(&((char*)storage)[to],tmp_md2->multidval.val,tmp_md2->multidval.totalsize);
				to += tmp_md2->multidval.totalsize;
				_NclDestroyObj((NclObj)tmp_md2);
				
				
			}
		}
		_NclDestroyObj(tmp_md);
		close(fd);
	}
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
	
	return(CcmReadVar(therec, dim_name,start,finish, stride, storage));
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
	CCMFileRec *thefile = (CCMFileRec*)therec;
	int i;
	NclQuark *arout;

	for(i = 0; i < thefile->n_vars; i++ ) {

		if(var_name == thefile->vars[i].var_name_q) {
/*
				arout[0] = NrmQuarkToString("long_name");
				arout[1] = NrmQuarkToString("units");
*/
			if(NrmStringToQuark("units") == att_name) {
				if(thefile->vars[i].ccm_var_index != -1) {
					*(NclQuark*)storage = NrmStringToQuark(ccm_name_tab[thefile->vars[i].ccm_var_index].udunit);
				} else {
					*(NclQuark*)storage = CcmVarName(&(thefile->header.cheader.MCFLDS[2*WORD_SIZE * i + WORD_SIZE]));
				}
			} else if(NrmStringToQuark("long_name") == att_name) {
				*(NclQuark*)storage = NrmStringToQuark(ccm_name_tab[thefile->vars[i].ccm_var_index].long_name);
			} else if(NrmStringToQuark("t_op") == att_name) {
				switch(thefile->vars[i].sample_type) {
				case 0:
					*(NclQuark*)storage = NrmStringToQuark("instantaneous");
					break;
				case 1:
					*(NclQuark*)storage = NrmStringToQuark("averaged");
					break;
				case 2:
					*(NclQuark*)storage = NrmStringToQuark("minimum_val");
					break;
				case 3:
					*(NclQuark*)storage = NrmStringToQuark("maximum_val");
					break;
				}
			}
			return(storage);
		}
	}
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
