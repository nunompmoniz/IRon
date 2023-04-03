/**

 ** The utility realted structures and functions prototypes.
 **   This helps the ansi compiler do tight checking.
 Rita P. Ribeiro

 **/

//#include "pchip.h"
#include "phi.h"


#ifndef FLOAT
#define FLOAT float
#endif

#ifdef MAINHT
#define EXTERN
#else
#define EXTERN extern
#endif

// typedef enum {false, true} bool;


typedef struct util_fun {
  double p;
  double Bmax;
  double event_thr;
} util_fun;


// static phi_fun *phiF;
static phi_bumps *bumpI;
static util_fun *utilF;

EXTERN void r2util(SEXP *n,
                   double *y,  double *ypred,
                   double *phiF_args,
                   double *loss_args,
                   double *utilF_args,
                   double *u);

EXTERN void r2util_init(double *phiF_args,
                        double *loss_args,
                        double *utilF_args);

EXTERN void r2util_eval(SEXP *n,
                        double *y,  double *ypred,
                        double *u);

EXTERN util_fun *util_init(double *utilF_args);

EXTERN phi_bumps *bumps_set(hermiteSpl *H, double *loss_args);

EXTERN void util_core(int n, double *y,  double *ypred,
                      phi_out *y_phiF, phi_out *ypred_phiF,
                      double *u);

EXTERN double util_value(double y, double ypred,
                         phi_out y_phiF, phi_out ypred_phiF,
                         phi_fun *phiF, phi_bumps *bumpI,
                         util_fun *utilF);

EXTERN void benefcost_lin(double y, double ypred,
                          double ypred_phi,
                          phi_fun *phiF, phi_bumps *bumpI,
                          double *lb, double *lc, double *ycphi);

