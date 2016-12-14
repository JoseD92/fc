void print(string s){
  write s;
}

void sortInt(int i){
  int mat[2][100],j,k,l,y,z,w,cantidad;

  print("\nIngrese cantidad de numeros a ordenar: ");
  read cantidad;
  if (cantidad>100){
    print("\nNumero muy grande.");
    return;
  }
  
  print("\nIngrese los numeros a ordenar: \n");
  i=0;
  while (i<cantidad){
    read j;
    mat[0][i]=j;
    i=i+1;
  }

  y=1;z=0;i=1;
  while(i<cantidad){
    w=0;
    while(w<cantidad){
      l=w;
      j=w;k=w+i;
      while(j<w+i && k<w+2*i){
        
      }
      while(j<w+i){
        mat[y][l]=mat[z][j];
        j=j+1;
        l=l+1;
      }
      while(k<w+2*i){
        mat[y][l]=mat[z][j];
        k=k+1;
        l=l+1;
      }
      w=w+2*i;
    }
    i=i*2;
    j=z;
    z=y;
    y=j;
  }
}

int main(int i){
  while(True){
    print("\nQuiere continuar (si=1)? ");
    read i;
    if (i!=1){break;}
    print("\nIndique que ordenar (enteros=1,flotantes=2): ");
    read i;
    if (i==1){sortInt(0);}
    //if (i==2){}
    gaus(0);
  }
}