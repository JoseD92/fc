int gcd(int a, int b){
  if (a==b){return a;}
  else {
    if(a<b){return (gcd(a,b-a));}
    else {return (gcd(a-b,b));}
  }
}

//int gcd(int a, int b){
//  if (a==b){return a;}
//  if(a<b){return (gcd(a-b,b));}
//  return (gcd(a,b-a));
//}

int main(int s){
  int x,y,z;
  read x;
  z = gcd(x,4);
  write z;
}