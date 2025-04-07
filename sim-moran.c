#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define RND drand48()
#define MAXT 100
#define NREP 1000
#define MAXN 3000

int main(void)
{
  double *hset;
  double ds, h0;
  int nstar;
  double lambda = 1;
  double nu = 0.1;
  double sumh, sumh2;
  double rates[4];
  double r, r2;
  FILE *fp;
  int w, m;
  int neww, newm;
  double t;
  int rep;
  double total;
  int i;
  int div;
  double dt;
  double meanh;
  
  hset = (double*)malloc(sizeof(double)*MAXT*MAXN*NREP);

  srand48(12);
  fp = fopen("output-moran.csv", "w");
  fprintf(fp, "ds,nstar,h0,t,Eh,SDh\n");
  ds = 0;
  for(ds = -0.5; ds <= 0.5; ds += 0.125)
    {
      for(nstar = 40; nstar < MAXN; nstar *= 2)
	{
          for(h0 = 0.1; h0 <= 0.9; h0 += 0.2)
	    {
	      printf("ds %f nstar %i h0 %f\n", ds, nstar, h0);
	      for(rep = 0; rep < NREP; rep++)
		{
		  w = round(nstar*(1.-h0));
		  m = round(nstar*h0);
		  for(div = 0; div < MAXT*nstar; div++)
		    {
		      hset[rep*MAXT*nstar+div] = (double)m/(w+m);
		      rates[0] = w*(lambda-ds*lambda);
		      rates[1] = m*(lambda+ds*lambda);
		      total = rates[0]+rates[1];
		      rates[0] /= total; rates[1] /= total;

		      r = RND; r2 = RND;
		      if(r < rates[0]) w++; else m++;
		      if(r2 < (double)w/(w+m)) w--; else m--;
		    }
		}
	      for(div = 0; div < MAXT*nstar; div++)
		{
		  if(div % nstar == 0) {
		    sumh = sumh2 = 0;
		    for(rep = 0; rep < NREP; rep++)
		      {
			sumh += hset[rep*MAXT*nstar+div];
			sumh2 += hset[rep*MAXT*nstar+div]*hset[rep*MAXT*nstar+div];
		      }
		    fprintf(fp, "%f,%i,%f,%i,%f,%f\n", ds, nstar, h0, div/nstar, sumh/NREP, sqrt( sumh2/NREP - (sumh/NREP)*(sumh/NREP) ));
		  }
		}
	    }
	}
    }
  fclose(fp);
  return 0;
}
