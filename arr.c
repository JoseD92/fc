int main(int i){
  int arr[10];
  int i,j;
  i=0;
  while (i<10){
    arr[i]=10-i;
    i=i+1;
  }
  i=0;
  while (i<10){
    j=arr[i];
    write j;
    i=i+1;
  }
}