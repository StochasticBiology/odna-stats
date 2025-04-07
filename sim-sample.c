#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define RND drand48()
#define MAXT 100
#define NREP 1000

int main(void)
{
  double *hset, *nset;
  double ds, h0;
  int nstar;
  double lambda = 1;
  double nu = 0.1;
  double sumh, sumh2;
  double sumn, sumn2;
  double rates[4];
  double r;
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
  
  hset = (double*)malloc(sizeof(double)*MAXT*NREP);
  nset = (double*)malloc(sizeof(double)*MAXT*NREP);

  srand48(12);
  fp = fopen("output-sample.csv", "w");
  fprintf(fp, "ds,nstar,h0,t,Eh,SDh\n");
  ds = 0;
  for(ds = -0.5; ds <= 0.5; ds += 0.125)
    {
      for(nstar = 40; nstar < 3000; nstar *= 2)
	{
          for(h0 = 0.1; h0 <= 0.9; h0 += 0.2)
	    {
	      printf("ds %f nstar %i h0 %f\n", ds, nstar, h0);
	      for(rep = 0; rep < NREP; rep++)
		{
		  w = round(nstar*(1.-h0));
		  m = round(nstar*h0);
		  for(div = 0; div < 100; div++)
		    {
		      hset[rep*MAXT+div] = (double)m/(w+m);
		      neww = newm = 0;
		      for(i = 0; i < nstar/2; i++)
			{
			  rates[0] = w*(lambda-ds*lambda);
			  rates[1] = m*(lambda+ds*lambda);
			  total = rates[0]+rates[1];
			  rates[0] /= total; rates[1] /= total;

			  r = RND;
			  if(r < rates[0]) neww++;
			  else newm++;
			}
		      w = neww; m = newm;
		    }
		}
	      for(div = 0; div < MAXT; div++)
		{
		  sumh = sumh2 = 0;
		  for(rep = 0; rep < NREP; rep++)
		    {
		      sumh += hset[rep*MAXT+div];
		      sumh2 += hset[rep*MAXT+div]*hset[rep*MAXT+div];
		    }
		  fprintf(fp, "%f,%i,%f,%i,%f,%f\n", ds, nstar, h0, div, sumh/NREP, sqrt( sumh2/NREP - (sumh/NREP)*(sumh/NREP) ));
		}
	    }
	}
    }
  fclose(fp);
  return 0;
}
