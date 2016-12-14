float raiz(float f){
  int i;
  float f2,fa;
  fa=f;
  f2 = f/3.0;
  i = 100000;
  while (i>0){
    f2=(f2+(f/f2))/2.0;
    i=i-1;
    if (fa-f2<0.000001){return f2;}
    fa = f2;
  }
  return f2;
}

int main(int i){
  string s;
  float f;
  s="\nQuiere Continuar (si=1): ";
  write s;
  read i;
  while(i==1){
    s="\nIndique numero a calcular raiz: ";
    write s;
    read f;
    f=raiz(f);
    write f;
    s="\nQuiere Continuar (si=1): ";
    write s;
    read i;
  }
}