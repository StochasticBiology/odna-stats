#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define RND drand48()
#define MAXT 1000
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
  double t;
  int rep;
  double total;
  int i;
  int tmpt, lastt, nextt;
  double dt;
  double meanh;
  int hist[101];
  
  FILE *fp1;
  
  hset = (double*)malloc(sizeof(double)*MAXT*NREP);
  nset = (double*)malloc(sizeof(double)*MAXT*NREP);

  srand48(12);
  fp = fopen("output-turnover.csv", "w");
  fprintf(fp, "ds,nstar,h0,t,Eh,SDh,En,SDn\n");
  fp1 = fopen("output-turnover-hist.csv", "w");
  fprintf(fp1, "ds,nstar,h0,t,h,Ph\n");
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
		  lastt = 0;
		  for(t=0;t<MAXT;)
		    {
		      rates[0] = w*(lambda-ds*lambda)*(1 - (w+m)/nstar);
		      rates[1] = m*(lambda+ds*lambda)*(1 - (w+m)/nstar);
		      rates[2] = w*nu;
		      rates[3] = m*nu;
		      total = 0;
		      for(i = 0; i < 4; i++) total += rates[i];
		      for(i = 0; i < 4; i++) rates[i] /= total;

		      dt = 1./total*log(1./RND);
		      if(total == 0 || t+dt > MAXT) {
			dt = MAXT-t;
		      }
		      if((int)(t+dt) > (int)t)
			{
			  for(lastt = (int)t; lastt <= (int)(t+dt); lastt++)
			    {
			      hset[rep*MAXT+lastt] = (double)m/(w+m);
			      nset[rep*MAXT+lastt] = (double)(w+m);
			    }
			}
		      t += dt;
		      
		      r = RND;
		      if(r < rates[0]) w++;
		      else if(r < rates[0]+rates[1]) m++;
		      else if(r < rates[0]+rates[1]+rates[2]) w--;
		      else m--;
		      if(w+m == 0 || w < 0 || m < 0) { printf("wtf\n"); }
		    }
		}
	      for(lastt = 0; lastt < MAXT; lastt++)
		{
		  sumh = sumh2 = 0;
		  sumn = sumn2 = 0;
		  for(i = 0; i < 101; i++) hist[i] = 0;
		  for(rep = 0; rep < NREP; rep++)
		    {
		      sumh += hset[rep*MAXT+lastt];
		      sumh2 += hset[rep*MAXT+lastt]*hset[rep*MAXT+lastt];
		      sumn += nset[rep*MAXT+lastt];
		      sumn2 += nset[rep*MAXT+lastt]*nset[rep*MAXT+lastt];
		      hist[(int)round(hset[rep*MAXT+lastt]*100)]++;
		    }
		  fprintf(fp, "%f,%i,%f,%i,%f,%f,%f,%f\n", ds, nstar, h0, lastt, sumh/NREP, sqrt( sumh2/NREP - (sumh/NREP)*(sumh/NREP) ), sumn/NREP, sqrt( sumn2/NREP - (sumn/NREP)*(sumn/NREP) ));
		  for(i = 0; i < 101; i++)
		    {
		      if(hist[i] != 0)
			fprintf(fp1, "%f,%i,%f,%i,%f,%i\n", ds, nstar, h0, lastt, (double)i/100., hist[i]);
		    }
		}
	    }
	}
    }
  fclose(fp);
  return 0;
}
