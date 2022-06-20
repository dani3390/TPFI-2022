#include <stdio.h>
#include <stdlib.h>


typedef struct{
   	  short unsigned int prec; 
   	  short unsigned int succ;
   	} Pair;


void printPrimes(Pair* p, int n){ //preso da classroom scorso anno
	for (int i=2; i<n; i+=p[i].succ) printf("%d ",i);
	printf("\n"); 	
}

Pair* eulerSieve(int n){
    Pair* p = (Pair*) calloc(n, sizeof(Pair));

    for (int i = 0; i < n; i++){
        p[i].prec = 1;
        p[i].succ = 1;
    }

    //levo i pari andandoli a 'saltare' nel for successivo
    for (int i = 5; i < n; i += 2) {
        p[i].prec = 2;
        p[i].succ = 2;
        
    }


    p[3].succ = 2;

    for (int i = 3; i * i < n; i += p[i].succ) {
    //mi fermo a un valore p * p > n (5 * 5 > 24)
        for (int j = i; i * j < n; j += p[j].succ) {
        // i = 3
        // 3 * 3 < 24
        // i += p[3].succ => 3 + 2

            for (int r = j * i; r < n; r *= i) {
                // t = 5 * 3
                // t < 24
                // t = t * p = 15 * 3 (dopo), ora vale 15

                p[r - p[r].prec].succ += p[r].succ;
                //p[15 - 2].succ += p[15].succ
                //p[13] = 4


                if (r + p[r].succ < n)
                    //15 + 2 < 24? Si
                    p[r + p[r].succ].prec += p[r].prec;
                    //p[15 + 2].prec += prime[15].prec
                    //p[17] = 4
            }
        }
    }


    return p;


}

int main() {
    int n = 24;
    Pair *p;

    p = eulerSieve(n);

    printf("La lista dei numeri primi di %d è ", n); 
    for (int i = 2; i < n; i += p[i].succ) {
        printf("%d ", i);
    }

    free(p);

    return 0;
}
