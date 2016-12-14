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

void estadisticas(int i){
   float f,muestras, promedio,varianza, max, min,desviacion;
   string s;
   s = "\nIndique numero de muestras: ";
   write s;
   read i;
   if (i<=0){
     s="Numero indicado muy pequeno";
     write s;
     return;
   }
   s = "\nIngrese las ";
   write s;
   write i;
   s= " muestras:\n";
   write s;

   muestras=0.0;
   promedio=0.0;
   varianza=0.0;
   min = 3.40282347E+38;
   max = -3.40282347E+38;
   while (i>0){
    read f;
    muestras=muestras+1.0;
    promedio=promedio+f;
    varianza=varianza+f*f;

    if (f>max){max=f;}
    if (f<min){min=f;}

    i=i-1;
  }
  promedio=promedio/muestras;
  varianza=(varianza/muestras)-(promedio*promedio);
  desviacion = raiz(varianza);
  
  s="\nNumero de muestras: ";
  write s;
  write muestras;
  s="\nPromedio: ";
  write s;
  write promedio;
  s="\nVarianza: ";
  write s;
  write varianza;
  s="\nDesviacion estandar: ";
  write s;
  write desviacion;

  s="\nMaximo: ";
  write s;
  write max;
  s="\nMinimo: ";
  write s;
  write min;
}

int main(int i){
  string s;
  int i;
  s="\nQuiere iniciar un calculo estadistico (si=1)? ";
  while(True){
    write s;
    read i;
    if (i!=1){break;}
    estadisticas(0);
  }
}