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
  string s,s2,s3;
  s = "\nQuiere continuar? (si=1,no=not 1) ";
  s2 = "\nSeleccione modo (recursivo=1,iterativo=not 1): ";
  s3 = "\nFactorial a calcular: ";
  write s;
  read ask;
  while(ask==1){
    write s2;
    read ask;
    write s3;
    read x;
    if (ask==1){i=fac(x);}
    else{i=fac2(x);}
    write i;
    write s;
    read ask;
  }
}