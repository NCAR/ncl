#include <stdlib.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "date.h"
#include "NclGRIB.h"
#include <math.h>


int grid_index[] = { 1, 2, 3, 4, 5, 6, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 33, 34, 37, 38, 39, 40, 41, 42, 43, 44, 45, 50, 55, 56, 61, 62, 63, 64, 75, 76, 77, 85, 86, 87, 90, 91, 92, 93, 98, 100, 101, 103, 104, 105, 106, 107, 126, 201, 202, 203, 204, 205, 206,207, 208, 209, 210, 211, 212, 213, 214 };

int grid_tbl_len = sizeof(grid_index)/sizeof(int);



#define EAR 6371.2213
#define PI 3.14159265
#define PI4  (PI/4)
#define RADDEG (180./PI)
#define EAST 1
#define WEST -1

static float er;
static float er2;
static float xax;
static float xpp;
static float ypp;

static void grdsetup(float x,float y,float gsp,float d,float ax) 
{
	er =  EAR * (1.0 + sin(d/RADDEG)) / gsp;
	er2 = er * er;
	xax = ax;
	xpp = x;
	ypp = y;
	return;
}


static void grdloc(float xp,float yp, float *xlo, float* xla)
{
	float r2,ss;
	float yy,xx,elong;


	yy = yp - ypp;
	xx = xp - xpp;
	*xlo = 0.0;
	if((yy != 0.0)||(xx!=0) ) {
		elong = RADDEG * atan2(yy,xx);
		*xlo =  elong + xax;
		if(*xlo > 180.0) 
			*xlo = *xlo - 360.0;
		if(*xlo < -180.0)
			*xlo = *xlo + 360;
	} 
	r2 = xx * xx + yy * yy;
	ss = (er2-r2)/(er2+r2);
	*xla = RADDEG * asin(ss);
} 

static printbinary(int val) {
	
	(val & 020000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 010000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 004000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 002000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 001000000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000400000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000200000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000100000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000040000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000020000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000010000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000004000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000002000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000001000000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000400000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000200000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000100000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000040000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000020000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000010000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000004000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000002000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000001000) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000400) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000200) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000100) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000040) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000020) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000010) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000004) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000002) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	(val & 000000000001) ? fprintf(stdout,"1") : fprintf(stdout,"0");
	fprintf(stdout,"\n");
}

void GetGrid_105
#if NhlNeedProto
(float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
	int xsize = 83;
	int ysize = 83;
	float polex = 40.5;
	float poley = 88.5;
	float dist = 90.75464;
	float deg = 60.0;
	float ore = -105.0;
	int x,y;
	
	
	*lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
	*dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
	*n_dims_lat = 2;
	*n_dims_lon = 2;
	(*dimsizes_lat)[0] = 83;
	(*dimsizes_lat)[1] = 83;
	(*dimsizes_lon)[0] = 83;
	(*dimsizes_lon)[1] = 83;

	grdsetup(polex,poley,dist,deg, ore + 90.0 );
	for (y = 0; y < ysize; y++) {
		for(x = 0; x < xsize; x++) {
			grdloc(x,y,&((*lon)[y * ysize + x]),&((*lat)[y * ysize + x]));
		}
	}
	
}
/*
* Grid dimensions must be set in var_info field of 
*/
static int GenericUnPack
#if NhlNeedProto
(int fd, void** outdat, void** missing_value, GribRecordInqRec *therec, GribParamList* thevarrec)
#else
(fd, outdat, missing_value, therec, thevarrec)
int fd;
void** outdat;
void** missing_value;
GribRecordInqRec *therec;
GribParamList* thevarrec;
#endif
{
	int index =0,i=0;
	int integer = 0;
	int spherical_harm = 0;
	int second_order = 0;
	int additional_flags = 0;
	int sign;
	char tmp[4];
	int number_of_bits;
	int binary_scale_factor;
	int decimal_scale_factor;
	int unused_bits;
	float reference_value;
	float tmpb,tmpa;
	char *bds;
	int total = 0;
	void *data = NULL;
	int isize = sizeof(int)*8;
	unsigned int X;
	int tbits;
	int bboff;
	


	bds = (char*)NclMalloc((unsigned)therec->bds_size);
	lseek(fd,therec->start + therec->bds_off,SEEK_SET);
	read(fd,(void*)bds,therec->bds_size);

	spherical_harm = (int)(bds[3] & (char)0200) ? 1 : 0;
	second_order = (int)(bds[3] & (char)0100) ? 1 : 0;
	integer = (int)(bds[3] & (char)0040) ? 1 : 0;
	additional_flags = (bds[3] & (char)0020) ? 1 : 0;


	tmp[0] = (therec->pds[26] & (char)0177);
	tmp[1] = therec->pds[27];
	decimal_scale_factor  = CnvtToDecimal(2,tmp);
	if(therec->pds[26] & (char)0200) 
		decimal_scale_factor = -decimal_scale_factor;

	number_of_bits = (int)bds[10];
	tmp[0] = (char)(bds[3] & (char)0017);
	unused_bits = CnvtToDecimal(1,tmp);
	tmp[0] = (char)(bds[4] & (char)0177);
	tmp[1] = bds[5];
	binary_scale_factor = CnvtToDecimal(2,tmp);
	if(bds[4] & (char)0200) {
		binary_scale_factor = -binary_scale_factor;
	}
	sign  = (bds[6] & (char) 0200)? 1 : 0;
	tmpa = (float)(bds[6] & (char)0177);
	tmpb = (float)CnvtToDecimal(3,&(bds[7]));

	reference_value = tmpb;
	reference_value *= (float)pow(2.0,-24.0);
	reference_value *= (float)pow(16.0,(double)(tmpa - 64));
	if(sign) {
		reference_value = -reference_value;
	}

	if((!spherical_harm)&&(!second_order)&&(!additional_flags)) {
		if(number_of_bits != 0) {
			i = 11;
			bboff = 0;
			index = 0;
			tbits = 0;
			total = (int)(((therec->bds_size - 11) * 8 - unused_bits)/ number_of_bits);
			if(integer) {
				data = (void*)NclMalloc((unsigned)sizeof(int)*total);
			} else {
				data = (void*)NclMalloc((unsigned)sizeof(float)*total);
			}
			while(index < total) {
				X = UnsignedCnvtToDecimal(4,&(bds[i]));
				fprintf(stdout,"o:");
				printbinary(X);
				X = X << bboff;
				fprintf(stdout,"l:");
				printbinary(X);
				X = X >> (isize - number_of_bits);
				fprintf(stdout,"r:");
				printbinary(X);
				fprintf(stdout,"(%d,%d,%d):\t%d\n",tbits,i,bboff,X);

				if(integer) {
					((int*)data)[index] = (int)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));

				} else {
					((float*)data)[index] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
				}
/*
				if((index > 1)&&(((float*)data)[index] > ((float*)data)[index-1] + 500)) {
					fprintf(stdout,"index:%d\n",index);
				} else if((index > 1)&&(((float*)data)[index] + 500 < ((float*)data)[index-1] )) {
					fprintf(stdout,"index:%d\n",index);
				}
*/

				tbits += number_of_bits;
				i = (int)(tbits/8.0) + 11;
				bboff = tbits % 8;
				index++;
			}
			*outdat = data;
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : constant field\n");
		}
	} else {
		if(spherical_harm)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Spherical Harmonics Detected can't un pack\n");
		if(second_order)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Second Order Detected can't un pack\n");
		*outdat = NULL;
		*missing_value = NULL;
	}
	return(integer);
}

GridInfoRecord grid[] = {
		NULL,NULL,"1679-point (73x23) Mercator grid with (0,0) at (0W,48.09S), (73,23) at (0W,48.09N); I increasing eastward, Equator at J=12. Grid increment of 5 degs of longitude", /*01*/
		NULL,NULL,"10512-point (144x73) global longitude-latitude grid.  (0,0) at 0E, 90N, latitude grid.  (0,0) at 0E, 90N, matrix layout.  N.B.: prime meridian not duplicated.", /*2*/
		NULL,NULL,"65160-point (360x181) global longitude-latitude grid.  (0,0) at 0E, 90N, matrix layout.  N.B.: prime meridian not duplicated.", /*3*/
		NULL,NULL,"259920-point (720x361) global lon/lat grid. (0,0) at 0E, 90N; matrix layout; prime meridian not duplicated", /*4*/
		NULL,NULL,"3021-point (53x57) N. Hemisphere stereographic grid oriented 105W; Pole at (27,49). (LFM analysis)",/*5*/
		NULL,NULL,"2385-point (53x45) N. Hemisphere polar stereographic grid oriented 105W; Pole at (27,49). (LFM Forecast)", /*6*/
		NULL,NULL,"1369-point (37x37) longitude-latitude grid. 0-180E, 0-90N", /*21*/
		NULL,NULL,"1369-point (37x37) longitude-latitude grid. 180W-0, 0-90N", /*22*/
		NULL,NULL,"1369-point (37x37) longitude-latitude grid. 0-180E, 90S-0", /*23*/
		NULL,NULL,"1369-point (37x37) longitude-latitude grid. 180W-0, 90S-0", /*24*/
		NULL,NULL,"1368-point (72x19) longitude-latitude grid. 0-355E, 0-90N", /*25*/
		NULL,NULL,"1368-point (72x19) longitude-latitude grid. 0-355E, 90S-0", /*26*/
		NULL,NULL,"4225-point (65x65) N. Hemisphere polar stereographic grid oriented 80W; Pole at (33,33).", /*27*/
		NULL,NULL,"4225-point (65x65) S. Hemisphere polar stereographic grid oriented 100E; Pole at (33,33).", /*28*/
		NULL,NULL,"5365-point (145x37) N. Hemisphere longitude/latitude grid for latitudes 0N to 90N; (0,0) at (0E,0N).", /*29*/
		NULL,NULL,"5365-point (145x37) S. Hemisphere longitude/latitude grid for latitudes 90S to 0S; (0,0) at (0E,90S).", /*30*/
		NULL,NULL,"8326-point (181x46) N. Hemisphere longitude/latitude grid for latitudes 0N to 90N; (0,0) at (0E,0N).", /*33*/
		NULL,NULL,"8326-point (181x46) S. Hemisphere longitude/latitude grid for latitudes 90S to 0S; (0,0) at (0E,90S).", /*34*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 60E-330E, 0-90N", /*37*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 150E-60E, 0-90N", /*38*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 240E-150E, 0-90N", /*39*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 330E-240E, 0-90N", /*40*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 60E-330E, 90S-0", /*41*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 150E-60E, 90S-0", /*42*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 240E-150E,90S-0", /*43*/
		NULL,NULL,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 330E-240E, 90S-0", /*44*/
		NULL,NULL,"41760-point (288x145) Global Latitude/Longitude 1.25 deg Resoulution. 0E-358.75E, 90N-90S",/*45*/
		NULL,NULL,"1188-point (36x33) longitude-latitude grid. 140.0W-52.5W, 20N-60N", /*50*/
		NULL,NULL,"6177-point (87x71) N. Hemisphere polar tereographic grid oriented 105W; Pole at (44,38). (2/3 bedient NH sfc anl)", /*55*/
		NULL,NULL,"6177-point (87x71) N. Hemisphere polar stereographic grid oriented 105W; Pole at (40,73). (1/3 bedient NA sfc anl)", /*56*/
		NULL,NULL,"4186-point (91x46) longitude-latitude grid. 0-180E, 0-90N", /*61*/
		NULL,NULL,"4186-point (91x46) longitude-latitude grid. 180W-0, 0-90N", /*62*/
		NULL,NULL,"4186-point (91x46) longitude-latitude grid. 0-180E, 90S-0", /*63*/
		NULL,NULL,"4186-point (91x46) longitude-latitude grid. 180W-0, 90S-0", /*64*/
		NULL,NULL,"12321-point (111x111) N. Hemisphere Lambert Conformal grid.  No fixed location; used by QLM Hurricane model.", /*75*/
		NULL,NULL,"12321-point (111x111) S. Hemisphere Lambert Conformal grid.  No fixed location; used by QLM Hurricane model.", /*76*/
		NULL,NULL,"12321-point (111x111) N. Hemisphere Mercator grid.  No fixed location; used by QLM Hurricane model.", /*77*/
		NULL,NULL,"32400-point (360x90) N. Hemisphere longitude/latitude grid; longitudes: 0.5E to 359.5E (0.5W); latitudes: 0.5N to 89.5N; origin (0,0) at (0.5E,0.5N)", /*85*/
		NULL,NULL,"32400-point (360x90) S. Hemisphere longitude/latitude grid; longitudes: 0.5E to 359.5E (0.5W); latitudes: 89.5S to 0.5S; origin (0,0) at (0.5E,89.5S)", /*86*/
		NULL,NULL,"5022-point (81x62) N. Hemisphere  polar stereographic grid oriented at 105W. Pole at (31.91, 112.53) Used for RUC.", /*87*/
		NULL,NULL,"12902-point (92x141 semi-staggered) lat. long., rotated such that center located at 52.0N, 111.0W; LL at 37.5W, 35S Unfilled E grid for 80 km ETA model", /*90*/
		NULL,NULL,"25803-point (183x141) lat. long., rotated such that center located at 52.0N, 111.0W; LL at 37.5W,35S Filled E grid for 80 km ETA model", /*91*/
		NULL,NULL,"24162-point (127x191 semi-staggered) lat. long., rotated such that center located at 41.0N, 97.0W; LL at 35W,25S Unfilled E grid for 40 km ETA model", /*92*/
		NULL,NULL,"48323-point (253x191)lat. long., rotated such that center located at 41.0N, 97.0W; LL at 35W ,25S Filled E grid for 40 km ETA model", /*93*/
		NULL,NULL,"18048-point (192x94) Global Gaussian T62 Latitude/Longitude Resolution.", /*98*/
		NULL,NULL,"6889-point (83x83) N. Hemisphere polar stereographic grid oriented 105W; Pole at (40.5,88.5). (NGM Original C-Grid)",  /*100*/
		NULL,NULL,"10283-point (113x91) N. Hemisphere polar stereographic grid oriented 105W; Pole at (58.5,92.5). (NGM \"Big C-Grid\")", /*101*/
		NULL,NULL,"3640-point (65x56) N. Hemisphere polar stereographic grid oriented 105W; Pole at (25.5,84.5) (used by ARL)", /*103*/
		NULL,NULL,"16170-point (147x110) N. Hemisphere polar stereographic grid oriented 105W; pole at (75.5,109.5). (NGM Super C grid)", /*104*/
		GenericUnPack,GetGrid_105,"6889-point (83x83) N. Hemisphere polar stereographic grid oriented 105W; pole at  (40.5,88.5).  (U.S. area subset of NGM Super C grid, used by ETA model)", /*105*/
		NULL,NULL,"19305-point (165x117) N. Hemisphere stereographic grid oriented 105W; pole at (80,176) Hi res. ETA (2 x resolution of Super C)", /*106*/
		NULL,NULL,"11040 point (120x92) N. Hemisphere stereographic grid oriented 105W; pole at (46,167) subset of Hi res. ETA; for ETA & MAPS/RUC", /*107*/
		NULL,NULL,"72960-point (384x190) Global Gaussian Latitude/Longitude T126 Resolution", /*126*/
		NULL,NULL,"4225-point (65x65) Hemispheric polar stereographic grid oriented 105W; pole at (33,33)", /*201*/
		NULL,NULL,"2795-point (65x43) National - CONUS polar stereographic oriented 105W; pole at (33,45)", /*202*/
		NULL,NULL,"1755-point (45x39) National - Alaska polar stereographic oriented 150W; pole at (27,37)", /*203*/
		NULL,NULL,"6324-point (93x68) National - Hawaii Mercator (0,0) is 25S,110E, (93,68) is 60.644S,109.129W", /*204*/
		NULL,NULL,"1755-point (45x39) National - Puerto Rico stereographic oriented 60W; pole at (27,57)", /*205*/
		NULL,NULL,"2091-point (51x41) Regional - Central MARD Lambert Conformal oriented 95W; pole at (30.00,169.745)", /*206*/
		NULL,NULL,"1715-point (49x35) Regional - Alaska polar stereographic oriented 150W; pole at 25,51", /*207*/
		NULL,NULL,"783-point (29x27) Regional - Hawaii mercator (0,0) is 9.343N,167.315W, (29,27) is 28.092N,145.878W", /*208*/
		NULL,NULL,"8181-point (101x81) Regional - Centeral US MARD - Double Res. Lambert Conformal oriented 95W; pole at (59.000,338.490)", /* 209*/
		NULL,NULL,"625-point (25x25) Regional - Puerto Rico mercator (0,0) is 9.000N,77.00W (25,25) is 26.422,58.625", /*210*/
		NULL,NULL,"6045-point (93x65) Regional - CONUS lambert conformal oriented 95W; pole at (53.000,178.745)", /*211*/
		NULL,NULL,"23865-point (185x129) Regional - CONUS - double resolution lambert conformal oriented 95W; pole at (105.000,256.490)", /* 212 */
		NULL,NULL,"10965-point (129x85) National - CONUS - Double Resolution polar stereographic oriented 105W; pole at (65,89)", /*213*/
		NULL,NULL,"6693-point (97x69) Regional - Alaska - Double Resolution polar stereographic oriented 150W; pole at (49,101)", /*214*/
};
