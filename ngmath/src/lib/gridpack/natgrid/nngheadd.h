void    Initialized(int, double [], double [], int, int,
                    double [], double []);

double  armind(int, double *);
double  armaxd(int, double *);

extern int     ReadDatad();
extern double  **MakeGridd(int, int, double *, double *);

extern void   c_nnsetrd(char *, double);
extern void   c_nngetrd(char *, double *);

extern void   Terminate();
