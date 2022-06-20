#include <stdio.h>
#include <stdlib.h>



int ulam(int n){
/* PREC: n>=0, POST: torna l’n-esimo numero di Ulam */

   int *p;
   p = (int*) calloc(n, sizeof(int)); 
   p[0] = 1;
   p[1] = 2;

   int num = 1;

   int i = 2;

   int count = 0;


    while (num != n) {
        i++;
        count = 0;
         //uk + 1 ≤ uk+1 ≤ uk−1 + uk

        //p[j] + 1 =< p[j+1] =< p[j-1] + p[j]
        
        for(int z = 0; z < i + 1; z++){
            
            for(int y = z + 1; y < i + 1; y++){

                    if (p[z] + p[y] == i){
                        count++;
                        if (count == 2) break;
                    }

            }
            if (count == 2) break; 

        }

        if (count == 1){
            num++;
            p[num] = i;
            //printf("%d ", i);
        }

    }

    int ret = p[n];

    free(p);

    return ret; 

}

int main() {

    int div;

    int val;

    div = ulam(4);
    printf("%d", div);

    //printf("Digita il valore: ");
    //scanf("%d", &val);

    //div = ulam(val);

    //printf("%d° numero di ulam: %d", val, div);

    return 0;

}