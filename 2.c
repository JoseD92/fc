union hola {
   int   holaid;
   int   algo;
};

struct Books {
   char  title[50];
   char  author[50];
   char  subject[100][2];
   int   book_id;
   struct Books *next;
   union hola unhola;
}; 

int masuno(int x,bool b){
  return x+1;
}

int main(int m){
   int x,**y;
   int a[6][5],c[5];
   struct Books b;
   x = 1;
   if (True) {1;} else {2;}
   while (True) {
      x=a[4][1];
      x=1;
      a[2]=c;
      a[2][2]=x+1;
      *y=*y;
      **y=a[2][2];
      x=x;
      x=b.book_id;
      x=masuno( b.unhola.algo+**y+(*(b.next)).book_id , True);
      b=b;
   };
}