#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define RND drand48()
#define MAXT 50
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
  int expt;
  double t;
  int rep;
  double total;
  int i;
  int tmpt, lastt, nextt;
  double dt;
  double meanh;
  
  FILE *fp1;
  
  hset = (double*)malloc(sizeof(double)*MAXT*NREP);
  nset = (double*)malloc(sizeof(double)*MAXT*NREP);

  srand48(12);
  fp = fopen("output-general.csv", "w");
  fprintf(fp, "expt,nstar,h0,t,Eh,SDh,En,SDn\n");
  ds = 0;
  for(expt = 0; expt < 5; expt++)
  // for(ds = -0.5; ds <= 0.5; ds += 0.125)
    {
      for(nstar = 40; nstar < 3000; nstar *= 2)
	{
          for(h0 = 0.1; h0 <= 0.9; h0 += 0.2)
	    {
	      printf("expt %i nstar %i h0 %f\n", expt, nstar, h0);
	      for(rep = 0; rep < NREP; rep++)
		{
		  w = round(nstar*(1.-h0));
		  m = round(nstar*h0);
		  lastt = 0; dt = 0;
		  for(t=0;t<MAXT;)
		    {
		      // cell divisions
		      if(t > 0 && (int)(t) % 10 == 0 && (int)(t-dt) % 10 != 0)
			{
			  //			  printf("division at %f\n", t);
			  switch(expt)
			    {
			    case 0: break;
			    case 1: w = round((double)w/2.)*2; m = round((double)m/2.); break;
			    case 2: w = round((double)w/5.)*2; m = round((double)m/5.); break;
			    case 3: neww = newm = 0;
			      for(i = 0; i < w; i++) neww += (RND < 0.5);
			      for(i = 0; i < m; i++) newm += (RND < 0.5);
			      w = neww; m = newm;
			      break;
			    case 4: neww = newm = 0;
			      for(i = 0; i < w; i++) neww += (RND < 0.2);
			      for(i = 0; i < m; i++) newm += (RND < 0.2);
			      w = neww; m = newm;
			      break;
			    }
			}

		      rates[0] = w*(lambda-ds*lambda)*(1 - (w+m)/nstar);
		      rates[1] = m*(lambda+ds*lambda)*(1 - (w+m)/nstar);
		      rates[2] = w*nu;
		      rates[3] = m*nu;
		      if(rates[0] < 0) rates[0] = 0;
		      if(rates[1] < 0) rates[1] = 0;
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
		      if(w < 0 || m < 0) { printf("wtf\n"); }
		    }
		}
	      for(lastt = 0; lastt < MAXT; lastt++)
		{
		  sumh = sumh2 = 0;
		  sumn = sumn2 = 0;
		  for(rep = 0; rep < NREP; rep++)
		    {
		      sumh += hset[rep*MAXT+lastt];
		      sumh2 += hset[rep*MAXT+lastt]*hset[rep*MAXT+lastt];
		      sumn += nset[rep*MAXT+lastt];
		      sumn2 += nset[rep*MAXT+lastt]*nset[rep*MAXT+lastt];
		    }
		  fprintf(fp, "%i,%i,%f,%i,%f,%f,%f,%f\n", expt, nstar, h0, lastt, sumh/NREP, sqrt( sumh2/NREP - (sumh/NREP)*(sumh/NREP) ), sumn/NREP, sqrt( sumn2/NREP - (sumn/NREP)*(sumn/NREP) ));
		}
	    }
	}
    }
  fclose(fp);
  return 0;
}
