#include <stdio.h>
#include <stdlib.h>

int* maxAltamenteComposto(int k, int* ac){ //
/* PREC: k>0, POST: torna un vettore d lungo k
* tale che d[i]=numero divisori di i>0 e carica in *ac
* il maggiore numero altamente composto < k.
*/
   int *d;
   int min = 0;
   int num = 0;

   d = (int*) calloc(k, sizeof(int));

   for (int i = 1; i <= k; ++i){
      for (int j = i; j <= k; j += i){
            ++d[j];
        }
  
      if (d[i] > min){
         min = d[i];
         num = i;
      }
    }


  *ac = num;

   //printf("%d\n", ac);

   return d;
}

int main() {
   int div = 0;
   int *f;
   int val;
   val = 31;
   //printf("Digita il valore: ");
   //scanf("%d", &val);

   // Per strani motivi visual studio code non mi permette di digitare dei valori

   

   f = (int*) calloc(val, sizeof(int));

   struct Node* lal = NULL;

   f = maxAltamenteComposto(val, &div);

   for (int i = 0; i < val; i++){
        printf("Numero di divisori di %d: %d\n", i, f[i]);
    }

   printf("ac = %d", div);

   free(f);

   return 0;

}