class IyteHashTable {
  val ArraySize = 10000000  //define array size
  //Runtime.getRuntime().totalMemory() //Int.MaxValue - 80 // 214748364
  val aHashMapArray = new Array[ Array[(String, String)] ](ArraySize.toInt) //create a hashmap array that keeps tuples of array

  //
  //
  //hash code function that calculates hash value and returns it
  private def HashCodeFunction(Key: String): Int = {
  
    val R = 31
    var HashValue = 0;
    
    //
    //
    // calculates hash value (multiply previous Hash Value, add it to char of i(th) index.)
    for (i <- 0 to Key.length() - 1) {
      HashValue = (R * HashValue + Key.charAt(i)) % ArraySize.toInt; 
      
      if (HashValue < 0)
        HashValue = 0
    }
    
    return HashValue
    
  }
  
  
  //
  //
  //set function adds or replace the new hash and (key,value)s
  def set(key: String, value: String): Unit = {
    try {
      
      val HashCode = HashCodeFunction(key) //create a hashcode for the key calculated by hashcodefunction
      var tupleArray = aHashMapArray(HashCode) //adding the hashCode to tupple array
      
      if (tupleArray == null) {  //check if tuplearray is null. If it is null we can directly equate the tupple array to hascode array
        
        tupleArray = new Array[(String, String)](0) 
        
        aHashMapArray(HashCode) = tupleArray
        
      }//
      
      aHashMapArray(HashCode) = AddOrReplace(tupleArray, (key, value)) //adding or replacing..
    
    }
    
    catch {
      
      case e: Exception => println("exception caught: " + e);
    
    }

  }

  private def AddOrReplace(aArray: Array[(String, String)], Key_Value: (String, String)): Array[(String, String)] = {
    var tmpTupleElement = ("", "")
    
    if (aArray.length == 0) {
      return addArrayAppend(aArray, Key_Value)
    }
    
    else {
      
      for (i <- 0 to (aArray.length - 1)) {
        tmpTupleElement = aArray(i)
        
        if (tmpTupleElement._1 == Key_Value._1) {
          aArray(i) = Key_Value
          
          return aArray
        }
        
      }
      
    }
    
    return addArrayAppend(aArray, Key_Value)
  
  }

  private def addArrayAppend(aArray: Array[(String, String)], Key_Value: (String, String)): Array[(String, String)] = {
    
    val newArrLength = aArray.length + 1
    val newArr = new Array[(String, String)](newArrLength)
   
    for (i <- 0 to (aArray.length - 1))
      newArr(i) = aArray(i) // 1
    
    newArr(aArray.length) = Key_Value
    
    return newArr
    
  }


  def get(key: String): String = {
    
    try {
      val HashCode = HashCodeFunction(key)
      val tupleArray = aHashMapArray(HashCode)
      var tmpTupleElement = ("", "")
      
      if (tupleArray == null)
        return null
      
      if (tupleArray.length == 0)
        return null
      
      for (i <- 0 to (tupleArray.length - 1)) {
        tmpTupleElement = tupleArray(i)
        
        if (tmpTupleElement._1 == key)
          return tmpTupleElement._2
      
      }
      
      return null
    }
    
    catch {
      case e: Exception => null;
    }
    
  }
  
}
object IyteHashTable
{
  def apply() = new IyteHashTable()
}
