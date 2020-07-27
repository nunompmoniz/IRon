#include <string.h>
#include "allocS.h" // ALLOC
#include "pchip.h"

#ifndef FLOAT
#define FLOAT float
#endif

#ifdef MAINHT
#define EXTERN
#else
#define EXTERN extern
#endif

#define IS_ZERO(a) fabs(a) < 0.000001 // double data type problems
#define CHECK_NORM(a,b) ((b) < 0.000001 ? 1.0 : (a)/(b))



typedef enum {extremes, range} phimethod;

#define DELTA 0.00001 // a value to avoid the null tradeoff of P and R

// this struct should be improved
typedef struct phi_out {
  double y_phi;
} phi_out;

// a piecewise cubic Hermite interpolant polynomial (self-contained)
typedef struct {
  phimethod method;
  hermiteSpl *H;
  phi_out  (*phi_value)();
} phi_fun;

typedef struct {
  int n;
  double *bleft;//x axis of left local min
  double *bmax;//x axis of local max
  double *bloss;//x axis of local max
} phi_bumps;

static phi_fun *phiF;

/* --------------------------------------------------------- */
/* Phi Function */
/* --------------------------------------------------------- */

EXTERN void r2phi(Sint *n, double *y,
                  double *phiF_args,
                  double *y_phi);

EXTERN void r2phi_init(double *phiF_args);

EXTERN void r2phi_eval(Sint *n, double *y,
                       double *y_phi);


		       
EXTERN void r2jphi_eval(Sint *n, double *y_phi, double *ypred_phi, double *p,
			double *jphi);


EXTERN phi_fun *phi_init(double *phiF_args);

EXTERN double jphi_value(double y_phi, double ypred_phi, double p);

EXTERN hermiteSpl *phiSpl_init(double *phiF_args);

EXTERN phi_out phiSpl_value(double y, phi_fun *phiF);
