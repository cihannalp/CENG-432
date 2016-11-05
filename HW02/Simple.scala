object Simple extends App{
  
  val myArray = Array(10, 25, 30);
  
  myArray.foreach( (n : Int) =>
  
      if(n % 2 == 0) println(2 * n) else println(3 * n)
    
    );
}
