/* cboxn.c  v1.0  SPAIN 02/02/2000
Author 1: Joaquin Ordieres Mere
          ordieres.mere@dim.unirioja.es
Author 2: Fco. Javier Martinez de Pison Ascacibar
          fjmartin@die.unirioja.es
Author 3: Manuel Castejon Limas
          manuel.castejon@dim.unirioja.es
Author 4: Francisco Javier de Cos Juez
          francisco-javier.de-cos@dim.unirioja.es

UNIVERSITY OF LA RIOJA
LOGRONYO(SPAIN)

          
########################
Obtain Number of boxes 
########################
*/


#define HASH_TAM 1000

#include <stdlib.h>
#include <stdio.h>
#include <math.h>



void cboxn(int *nrow, int *ncol, int *minPBox, int *xmat, 
		 double *rad, double *SumSQRFreqs, double *Informations, int *NBoxes,
		double *q, double *Dq);


 
void cboxn(int *nrow, int *ncol, int *minPBox, int *xmat, 
		 double *rad, double *SumSQRFreqs, double *Informations, int *NBoxes,
		double *q, double *Dq)
{
int *point;
int h,j,k,RadDiv;
double invrad,Freq,nvec;
int nco, nro,NotFind, NotEqual;
int NumE,NumPoint,TamHash,NumBox,PosP,PointInBox;
nco=*ncol;
nro=*nrow;

/* Initializate table HASH */
point=(int *)calloc(HASH_TAM*(nco+1),sizeof(int));
TamHash=HASH_TAM;
if (!point)
 {
   	printf("Can not allocate specified workspace\n");
	exit(0);
 }

if (sizeof(int)<4)
{
	printf("Sizeof int <4\n");
	exit(1);
}

if (nro<2)
{
	printf("Number of points <2\n");
	exit(2);
}

NumPoint=0;

RadDiv=1000000000*(*rad);
/*printf("%d\n",RadDiv);*/
invrad=(1.0)/(*rad);
	/*
		printf("InvRadio=%f\n",invrad);
	*/


/* Divide points in boxes */
for (h=0;h<nro;h++)
 {
   	NumE=h*nco;	

	NotFind=1;
	for (j=0;j<NumPoint;j++)
	{
		PosP=j*(nco+1);
		NotEqual=0;		
		for (k=0;k<nco;k++)
		{
			if ((*(point+PosP+k))!=((*(xmat+k+NumE))/RadDiv))
				{
				NotEqual=1;
				break;
				}
		}
		if (!NotEqual)
		{
			(*(point+PosP+nco))++;
			NotFind=0;
			break;
		}
	}
	
	if (NotFind)
	{
		PosP=NumPoint*(nco+1);
		for (k=0;k<nco;k++)
		{
			(*(point+PosP+k))=(*(xmat+k+NumE)/RadDiv);
		}
		(*(point+PosP+nco))=1;
		NumPoint++;
		if (NumPoint>=TamHash)
		{
			/* Realloc table HASH */
			/* Initializate table HASH */
			TamHash+=HASH_TAM;
			point=(int *)realloc(point,(TamHash*(nco+1)*sizeof(int)));
			if (!point)
 			{
			   	printf("Can not allocate specified workspace\n");
				exit(0);
			 }
	/*			 else
				 {
					printf("!!Realloc Good!!\n");
				 }
	*/

		}
	}
	
	/*
		printf("NumPoint=%d\n%d\t%d\t%d\t%d\t\t%d\t%d\n",NumPoint-1,
				*(point+PosP),*(point+PosP+1),*(point+PosP+2),*(point+PosP+3),
				*(point+PosP+4),h);
		getchar();
	*/
	
	
 }

NumBox=0;
nvec=(double)nro;
*SumSQRFreqs=0.0;
*Informations=0.0;
*Dq=0;
for (h=0;h<NumPoint;h++)
{
	PosP=h*(nco+1);
	PointInBox=*(point+PosP+nco);
	if (PointInBox>=(*minPBox))
		{
		Freq=PointInBox/nvec;
		*SumSQRFreqs+=(Freq*Freq);
		*Informations+=Freq*(log(Freq)/log(2));

		NumBox++;
		*Dq+=pow(Freq,*q);
		}
}

		
free(point);
*NBoxes=NumBox;
return;
}



