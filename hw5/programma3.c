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


    while (num + 1 != n) {
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

    int ret = p[n - 1];

    free(p);

    return ret; 

}

typedef struct Node{
	int data;
	struct Node* next;
	struct Node* prev;
} Node;


typedef struct listDCFirstLast{
   struct Node* first;
   struct Node* last;
} lista;

int size(struct Node *node)
{


   int res = 1; //non propriamente corretto, faccio partire da 1 per ottenere l'ulam number corretto
   while (node != NULL)
   {
       res++;
       node = node->next;
   }
   return res;
}

typedef lista* listDCFirstLast;

struct listDCFirstLast listo;

Node* initialize(){ //struct Node** head_ref

   Node* init = NULL;

   append(&init, 1);
   listo.first = init;

   append(&init, 2);
   listo.last = init;

   return init;

}

void append(struct Node** head, int data){

    struct Node* Node1 = (struct Node*) malloc(sizeof(struct Node));

    Node1->data = data;
    Node1->next = NULL;

    if(*head==NULL){
        *head = Node1;
        Node1->prev = NULL;
        return;
    }
    
    struct Node* temp = *head;
    
    while(temp->next!=NULL) temp = temp->next;
    
    temp->next = Node1;
    Node1->prev = temp;
    listo.last = Node1; 

}

void revTraversal(struct Node* node){
    printf("\nAttraversamento da ultimo a primo nodo\n");
    while (node != NULL){
        printf(" %d ", node->data);
        node = node->prev;
    }
}

void traversal(struct Node* node){
    printf("\nAttraversamento da primo a ultimo nodo\n");
    while (node != NULL) {
        printf(" %d ", node->data);
        node = node->next;
    }
}

int nextU(listDCFirstLast U){
/* PREC: U contiene ordinatamente
i primi k>=2 elementi della successione u */
   int val;

   val = ulam(size(U));

   append(&U, ulam);

   return val;

}

int main() {

    struct Node* head;
    
    head = initialize();


    append(&head,ulam(size(head)));
    append(&head,ulam(size(head)));
    append(&head,ulam(size(head)));
    append(&head,ulam(size(head)));
    append(&head,ulam(size(head)));
    append(&head,ulam(size(head)));
    append(&head,ulam(size(head)));
    append(&head,ulam(size(head)));

    traversal(head);
    traversal(listo.first);
    revTraversal(listo.last);

    return 0;
}