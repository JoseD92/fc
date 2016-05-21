int main(){
   int *x;
   x = &main;
   while (1) {
      printf("%u %i\n", x, *x);
      x++;
   };
}