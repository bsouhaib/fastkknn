/*****************************************************************/
/*
 *  Copyright (C)2009-2011 Klaus Schliep
 *               
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

 
#include <R.h>
#include <Rmath.h>
#include <R_ext/Memory.h> 
#include <R_ext/Print.h>  
#include <R_ext/Utils.h>



#define MIN(x,y)  (((x)<(y)) ?    (x)  : (y))
#define MAX(x,y)  (((x)>(y)) ?    (x)  : (y))

double eps = 1.0e-8, big = 1.0e300;


void dm(double *learn, double *valid, int *n, int *m, int *p, double *dm, int *cl, int *k, double *mink, double *weights){
    int i, j, l, t, nn, ii, kk; 
    double tmp, *dvec, maxD;
    int *cvec;

    kk = MAX(10L, *k);
    kk = MIN(kk, *n);
    nn = MIN(2L*kk, *n);

    cvec = (int *) R_alloc(nn, sizeof(int));
    dvec = (double *) R_alloc(nn, sizeof(double));
    for(t=0;t<nn;t++)dvec[t]= big;
  
    for(j=0;j<(*m);j++){
        i=0;
        ii=0L; 
        maxD = big;
	while(i<*n){
	    tmp=0.0;
            l=0; 
	    while(l<*p && tmp < (maxD+eps)){
	        tmp+=pow(fabs(learn[i+l*n[0]]-valid[j+l*m[0]]),*mink)* weights[l];
                l++; 
	    }

            if(tmp < maxD){
	        dvec[ii]=tmp;
	        cvec[ii]=i;
                ii++;
            }
            if( ii==(nn-1L) ){  
                rsort_with_index(dvec, cvec, nn);
                ii= *k-1L;
                maxD = dvec[*k-1L]; 
            }
            i++;         
	}
        rsort_with_index(dvec, cvec, nn);
        for(t=0;t<*k;t++){
            cl[j+t * *m]=cvec[t];
            dm[j+t * *m]=pow(dvec[t],(1.0/(*mink)));
        }
    }
}


void dmEuclid(double *learn, double *valid, int *n, int *m, int *p, double *dm, int *cl, int *k, double *weights){
    int i, j, l, t, nn, ii, kk; 
    double tmp, *dvec, maxD;
    int *cvec;

    kk = MAX(10L, *k);
    kk = MIN(kk, *n);
    nn = MIN(2L*kk, *n);

    cvec = (int *) R_alloc(nn, sizeof(int));
    dvec = (double *) R_alloc(nn, sizeof(double));
    for(t=0;t<nn;t++)dvec[t]= big;
  
    for(j=0;j<(*m);j++){
        i=0;
        ii=0L; 
        maxD = big;
	while(i<*n){
	    tmp=0.0;
            l=0; 
	    while(l<*p && tmp < (maxD+eps)){
	        tmp+=pow(learn[i+l*n[0]]-valid[j+l*m[0]],2.0)* weights[l];
                l++; 
	    }

            if(tmp < maxD){
	        dvec[ii]=tmp;
	        cvec[ii]=i;
                ii++;
            }
            if( ii==(nn-1L) ){  
                rsort_with_index(dvec, cvec, nn);
                ii= *k-1L;
                maxD = dvec[*k-1L]; 
            }
            i++;         
	}
        rsort_with_index(dvec, cvec, nn);
        for(t=0;t<*k;t++){
            cl[j+t * *m]=cvec[t];
            dm[j+t * *m]=sqrt(dvec[t]);
        }
    }
}

void knnpred(double *bigCL,double *D, double *W2, double * res,int *ntest,int *kinput,int *inc,int *allk)
{
	int k, i,j,kinit,limitk,step;
	double temp,sumw,r,maxdisti, W;
	
	if(*allk==1)
	{
		kinit=0; limitk=*kinput; step=*inc;
	}else {
		kinit=*kinput-1; limitk=*kinput; step=1;
	}

	
	for (k=kinit; k<limitk; k=k+step) {

		for (i=0; i<*ntest; i++) {
			
			sumw=0.0;
			temp=0.0;
			maxdisti=D[i+(k+1)**ntest];
			
			if(maxdisti<1.0e-6)
				maxdisti=1.0e-6;
			
			for (j=0; j<=k; j++) {
				
				W=D[i+j**ntest]/maxdisti;
				
				if(W>1-(1e-6))
					W=1-(1e-6);
						
				if(W<1e-6)
					W=1e-6;
						
				r=dbeta((W+1.0)/2.0,3,3,0);
				W2[i+j**ntest]=r;
				sumw=sumw+r;
				temp=temp+bigCL[i+j**ntest]*r;
			}
			res[k+i**kinput]=temp/sumw;
		}
	}
}
//W2[i+j**ntest]=D[i+j**ntest]/D[i+(k+1)**ntest];
//r=W2[i+j**ntest];
//temp=temp/sumw;


