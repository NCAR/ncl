/*
 *  Function prototypes for the natgrid package.
 */
void    c_nnseti(char *, int);
void    c_nngeti(char *, int *);
void    c_nnsetr(char *, float);
void    c_nngetr(char *, float *);
void    c_nnsetc(char *, char *);
void    c_nngetc(char *, char *);
void    c_nngetslopes(int, int, float *, int *);
void    c_nngetaspects(int, int, float *, int *);
void    c_nnpntinits(int, float [], float [], float []);
void    c_nnpnts(float, float, float *);
void    c_nnpntend();
float   *c_natgrids(int, float [], float [], float [],
                     int, int, float [], float [], int *);

void    c_nnsetrd(char *, double);
void    c_nngetrd(char *, double *);
void    c_nngetsloped(int, int, double *, int *);
void    c_nngetaspectd(int, int, double *, int *);
void    c_nnpntinitd(int, double [], double [], double []);
void    c_nnpntd(double, double, double *);
void    c_nnpntendd();
double  *c_natgridd(int, double [], double [], double [],
                     int, int, double [], double [], int *);
