struct asinfod
{  int          crows;
   int          ccols;
   double       **aspect_outd;
   double       **slope_outd;
};
struct asinfod  curasd;

extern double   armind(int, double *);
extern double   armaxd(int, double *);

extern void     Initialized(int, double [], double [], int, int,
                            double [], double []);

int             ReadDatad(int, double *, double *, double *);
double          **MakeGridd(int, int, double *, double *);

void            c_nngetsloped(int, int, double *, int *);
void            c_nngetaspectd(int, int, double *, int *);
void            c_nnpntinitd(int, double [], double [], double []);
extern void     c_nnpntd(double, double, double *);
void            c_nnpntendd();
