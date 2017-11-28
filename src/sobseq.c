#include "sobseq.h"

#define sobseq_err(msg) {printf("sobseq_err: %s.\n",msg); exit(255);}
#define MAXDIM 6
#define MAXBIT 62

inline
void sobseq(double *x, const int n, const int set)
/* NB: x runs from 0..n-1, NOT NR's usual 1..n */
/* n is dim of system, set => initialize if == 0 */
{
	int j,k,l;
	unsigned long long i,im,ipp;
	static double fac;
	static unsigned long long in,ix[MAXDIM],*iu[MAXBIT+1];
	static unsigned long long mdeg[MAXDIM+1]={0,1,2,3,3,4,4};
	static unsigned long long ip[MAXDIM+1]={0,0,1,1,2,1,4};
	static unsigned long long iv[MAXDIM*MAXBIT+1],ivb[MAXDIM*MAXBIT+1]={
		0,1,1,1,1,1,1,3,1,3,3,1,1,5,7,7,3,3,5,15,11,5,15,13,9
	};
	
	if (!set) {
		if (n < 1 || n > MAXDIM) 
			sobseq_err("dim parameter is out of range");
		for (k = 0; k <= MAXDIM*MAXBIT; k++)
			iv[k] = ivb[k];
		for (j=1,k=0;j<=MAXBIT;j++,k+=MAXDIM)
			iu[j] = &iv[k];
		for (k=1;k<=MAXDIM;k++) {
			ix[k-1] = 0;
			for (j=1;j<=(int)mdeg[k];j++) iu[j][k] <<= (MAXBIT-j);
			for (j=mdeg[k]+1;j<=MAXBIT;j++) {
				ipp=ip[k];
				i=iu[j-mdeg[k]][k];
				i ^= (i >> mdeg[k]);
				for (l=mdeg[k]-1;l>=1;l--) {
					if (ipp & 1) i ^= iu[j-l][k];
					ipp >>= 1;
				}
				iu[j][k]=i;
			}
		}
		fac = sobseq_boxlength/(((unsigned long long)1) << MAXBIT);
		in = 0;
	}
    im=in;
    for (j = 0;j < MAXBIT;j++) {
        if (!(im & (unsigned long long)1)) break;
	im >>= 1;
    }

#if SOBSEQ_ERRCHK
    if (j == MAXBIT)
        sobseq_err("MAXBIT too small");
#endif

    im = j*MAXDIM + 1;
    for (k = 0; k < n; k++) {
        ix[k] ^= iv[im+k];
        x[k]=ix[k]*fac;
    }
    in++;
}


#undef MAXDIM
#undef NONZEROBIT
#undef sobseq_err
