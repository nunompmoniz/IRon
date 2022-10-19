
/* ============================================================ */
/*                                                              */
/*    bump information   */
/*                                                              */
/* ============================================================ */
/*


 */
/*
 ** phi relevance function.
 **  - Cubic Hermite Spline
 ** Rita P. Ribeiro
 */

#include <stdio.h>
#include <stdlib.h> //malloc
#include <math.h>

//#include "R.h"
#include <R_ext/Applic.h> // to use findInterval

#include "allocS.h" // ALLOC
#include "util.h"

// MUST BE IMPROVED
phi_bumps *bumps_set(hermiteSpl *H, double *loss_args) {

  int i;
  phi_bumps *B;
  double sum_b = 0.0, delta = 0;
  double d1;
  int inBump = 1, j;
  int nB = H->npts, nb;
  int *critical_idx;

  if((critical_idx = (int *)ALLOC(nB,sizeof(int))) == NULL) perror("bump.c: memory allocation error");
  if((B = (phi_bumps *)ALLOC(1,sizeof(phi_bumps))) == NULL) perror("bump.c: memory allocation error");

  B->n = 0;
  if((B->bleft = (double *)ALLOC(nB,sizeof(double))) == NULL) perror("bump.c: memory allocation error");
  if((B->bmax = (double *)ALLOC(nB,sizeof(double))) == NULL) perror("bump.c: memory allocation error");
  if((B->bloss = (double *)ALLOC(nB,sizeof(double))) == NULL) perror("bump.c: memory allocation error");

  B->bleft[0] = -INFINITY;
  B->bmax[0] = -INFINITY;
  B->bloss[0] = INFINITY;

  j = 0;
  for(i = 0; i < H->npts; i++) {
    if(fabs(H->b[i]) == 0) {
      critical_idx[j] = i;
      j++;
    }
  }

  sum_b = H->x[critical_idx[0]];
  nb = 1;

  i = 0;

  while(i < j - 1) {


    d1 = H->a[critical_idx[i+1]] - H->a[critical_idx[i]];

    if(d1 == 0) {
      sum_b += H->x[critical_idx[i+1]];
      nb++;

    } else {

      if(d1 < 0 && inBump) { // update global max of the bump

        B->bmax[B->n] = sum_b / nb;

        if(R_FINITE(B->bmax[B->n]) && R_FINITE(B->bleft[B->n])) {
          B->bloss[B->n] = fabs(B->bmax[B->n] - B->bleft[B->n]);
        }

        inBump = 0;

      } else if(d1 > 0 && (!inBump || !B->n)) { // update global min of the new bump

        B->n++;

        B->bleft[B->n] = sum_b / nb;

        if(R_FINITE(B->bmax[B->n-1]) && R_FINITE(B->bleft[B->n])) {
          delta = fabs(B->bmax[B->n-1] - B->bleft[B->n]);
          if(delta < B->bloss[B->n-1])
            B->bloss[B->n-1] = 2*delta;
          else
            B->bloss[B->n-1] = 2*B->bloss[B->n-1];
        }


        inBump = 1;

      }

      sum_b = H->x[critical_idx[i+1]];
      nb = 1;
    }

    i++;
  } // while

  if(B->n > 0) { // still updates to be done

    if(inBump) {

      B->bmax[B->n] = sum_b / nb;

      if(R_FINITE(B->bmax[B->n]) && R_FINITE(B->bleft[B->n])) {
        B->bloss[B->n] = 2*fabs(B->bmax[B->n] - B->bleft[B->n]);
      }


    } else {

      B->n++;
      B->bleft[B->n] = sum_b / nb;
      B->bmax[B->n] = INFINITY;

      if(R_FINITE(B->bmax[B->n-1]) && R_FINITE(B->bleft[B->n])) {
        delta = fabs(B->bmax[B->n-1] - B->bleft[B->n]);
        if(delta < B->bloss[B->n-1])
          B->bloss[B->n-1] = 2*delta;
        else
          B->bloss[B->n-1] = 2*B->bloss[B->n-1];
      }
    }

    //the extrapolation is constant, and so it is bloss outside the range of control points
    if(!R_FINITE(B->bmax[0])) B->bloss[0] = B->bloss[1];
    if(!R_FINITE(B->bmax[B->n])) B->bloss[B->n] = B->bloss[B->n-1];

  } else { // for standard regression

    B->bloss[0] = loss_args[2];
  }

  B->n++;
  B->bleft[B->n] = '\0';
  B->bmax[B->n] = '\0';
  B->bloss[B->n] = '\0';

  // cannot free this!
  //free(critical_idx);
  return B;
}
