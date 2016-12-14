void print(string s){
  write s;
}

void gaus(int i){
  float mat[10][11],temp;
  int j,z,ecu;
  
  print("\nIndique numero de ecuaciones: ");
  read ecu;

  //i=0;j=0;
  //while(i<10){
  //  while(j<11){
  //    mat[i][j]=0.0;
  //    j=j+1;
  //  }
  //  i=i+1;
  //  j=0;
  //}

  print("\nIndique coeficientes de ecuaciones: \n");
  i=0;j=0;
  while(i<ecu){
    while(j<ecu){
      print("\nEcuacion ");
      write i;
      print(" coeficiente ");
      write j;
      print(": ");
      read temp;
      mat[i][j]=temp;
      j=j+1;
    }
    print("\nEcuacion ");
    write i;
    print(" resultado: ");
    read temp;
    mat[i][10]=temp;
    i=i+1;
    j=0;
  }

  i=0;j=0;z=0;
  while(i<ecu){
    while(z<ecu){
      if(z!=i){
        temp=mat[z][i]/mat[i][i];
        while(j<ecu){
          mat[z][j]=mat[z][j]-(mat[i][j]*temp);
          j=j+1;
        }
        mat[z][10]=mat[z][10]-(mat[i][10]*temp);
        j=0;
      }
      z=z+1;
    }
    z=0;
    i=i+1;
  }

  print("\n");
  i=0;j=0;
  while(i<ecu){
    while(j<ecu){
      temp=mat[i][j];
      write temp;
      print(" ");
      j=j+1;
    }
    temp=mat[i][10];
    write temp;
    print("\n");
    i=i+1;
    j=0;
  }


}

int main(int i){
  while(True){
    print("\nQuiere continuar (si=1)? ");
    read i;
    if (i!=1){break;}
    gaus(0);
  }
}