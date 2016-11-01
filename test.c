int gcd(int a, int b){
  if (a==b){return a;}
  else {
    if(a<b){return (gcd(a-b,b));}
    else {return (gcd(a,b-a));}
  }
}

//int gcd(int a, int b){
//  if (a==b){return a;}
//  if(a<b){return (gcd(a-b,b));}
//  return (gcd(a,b-a));
//}

int main(int s){
  int x,y,z;
  //write("Hola");
  z = gcd(x,y);
}