int fac(int n){
  int x;
  if (n==1){return 1;}
  x=fac(n-1);
  x=x*n;
  return x;
}

int fac2(int n){
  int i;
  i=1;
  while(n>1){
    i=i*n;
    n=n-1;
  }
  return i;
}

int main(int i){
  int x,ask;
  read ask;
  while(ask==1){
    read ask;
    read x;
    if (ask==1){i=fac(x);}
    else{i=fac2(x);}
    write i;
    read ask;
  }
}