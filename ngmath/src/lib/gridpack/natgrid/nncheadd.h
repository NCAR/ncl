#include <stdio.h>
#include <stdlib.h>   /* added 4 Nov. 94 */
#include <math.h>
#include <malloc.h>

#define SQ(x)   (x) * (x)
#define BIGNUM  1E37
#define EPSILON 0.00001           
#define RANGE   10
#define EQ      ==
#define NE      !=
#define AND     &&
#define OR      ||

struct datum                           
{  double       values[3];
   struct datum *nextdat;
};
struct datum    *rootdat = NULL, *curdat, *holddat;

struct simp
{  int          vert[3];
   double       cent[3];
   struct simp  *nextsimp;
};
struct simp     *rootsimp = NULL, *cursimp, *holdsimp, 
                *lastsimp, *prevsimp;

struct temp
{  int          end[2];
   struct temp  *nexttemp;
};
struct temp     *roottemp = NULL, *curtemp, *lasttemp, 
                *prevtemp;

struct neig
{  int          neinum;
   double       narea;
   double       coord;
   struct neig  *nextneig;
};
struct neig     *rootneig = NULL, *curneig, *lastneig;

struct asinfo
{  int          crows;
   int          ccols;
   double       **aspect_out;
   double       **slope_out;
};
struct asinfo   curas;

double          **points, **joints, wbit,
                magx = 1, magy = 1, magz = 1, maxxy[2][3],
                magx_auto, magy_auto, magz_auto,
                magx_orig, magy_orig, magz_orig,
                maxhoriz, aaa, bbb, ccc, det, 
                work3[3][3], xx, sumx, sumy, sumz, 
                sumx2, sumy2, sumxy, sumxz, sumyz, 
                asum, pi, piby2, piby32, rad2deg, 
                bigtri[3][3], horilap_save, vertlap_save;
extern double   horilap, vertlap, bI, bJ, nuldat, xstart, ystart, xend, yend;
int             datcnt, datcnt3, numtri, imag, numnei, iscale,
                ext, *jndx, neicnt, optim = 1, goodflag, updir = 1,
                scor[3][2] = {{1,2}, {2,0}, {0,1}}, auto_scale = 1,
                single_point = 0, first_single = 1, asflag = 1;
char            tri_file[256] = {"nnalg.dat"}, error_file[256] = {"stderr"},
                emsg[256];
extern int      igrad, extrap, non_neg, densi, sdip, rads, adf, 
                error_status, dup;
extern FILE     *filer, *filee;

extern void     Terminate();
extern double   armind(int, double *);
extern double   armaxd(int, double *);

extern void     ErrorHnd(int, char *, FILE *, char *);
extern void     Initialized(int, double [], double [], int, int,
                            double [], double []);

int             ReadData(int, double *, double *, double *);
void            FindNeigh();
void            TriCentr();
void            TriNeigh();
void            Gradient();
double           **MakeGrid(int, int, double *, double *);
void            FindProp();
double          Surface();
double          Meld();
void            TooSteep();
void            TooShallow();
void            TooNarrow();
struct datum    *IMakeDatum();
struct simp     *IMakeSimp();
struct temp     *IMakeTemp();
struct neig     *IMakeNeig();
int             *IntVect();
void            FreeVecti();
double          *DoubleVect();
void            FreeVectd();
int             **IntMatrix();
void            FreeMatrixi();
float           **FloatMatrix();
void            FreeMatrixf();
double          **DoubleMatrix();
void            FreeMatrixd();
void            c_nngetsloped(int, int, double *, int *);
void            c_nngetaspectd(int, int, double *, int *);
void            c_nnpntinitd(int, double [], double [], double []);
extern void     c_nnpntd(double, double, double *);
void            c_nnpntendd();
