/* pointdif.c  v1.0  SPAIN 18/02/2000
Author 1: Joaquin Ordieres Mere
          ordieres.mere@dim.unirioja.es
Author 2: Fco. Javier Martinez de Pison Ascacibar
          fjmartin@die.unirioja.es
Author 3: Manuel Castejon Limas
          manuel.castejon@dim.unirioja.es
Author 4: Francisco Javier de Cos Juez
	  francisco-javier.de-cos@dim.unirioja.es 

UNIVERSITY OF LA RIOJA
LOGROÑO(SPAIN)

          
###########################
Obtain diferent points
###########################
*/


#define HASH_TAM 1000

#include <stdlib.h>
#include <stdio.h>



void pointdif(int *nrow, int *ncol, int *xmat, 
		int *NumpDif);


void pointdif(int *nrow, int *ncol, int *xmat, 
		int *NumpDif)
{
int *point;
int h,j,k;
int nco, nro,NotFind, NotEqual;
int NumE,NumPoint,TamHash,PosP;
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
			if ((*(point+PosP+k))!=((*(xmat+k+NumE))))
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
			(*(point+PosP+k))=(*(xmat+k+NumE));
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

		}
	}
}
free(point);
*NumpDif=NumPoint;
return;
}



