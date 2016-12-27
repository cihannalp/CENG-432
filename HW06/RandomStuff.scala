trait RandomStuffTrait {
  
  def transform(list: List[Int], op: (Int) => Int): List[Int];
  def allValid(list: List[Int], op: (Int) => Boolean): Boolean;
  def executeWithRetry(retryCount: Int, op: => Int): Option[Int];
}

object RandomStuff extends RandomStuffTrait {
  
  def transform(list: List[Int], op: (Int) => Int): List[Int] = {
    var newList: List[Int] = List();  
    list.foreach({ item => newList = newList :+ op(item) });
    newList
  }
  
  def allValid(list: List[Int], op: (Int) => Boolean): Boolean = {
    var returnValue: Boolean  = true
    list.foreach(item => 
      if (!op(item)) {
      returnValue = false;
    });
    returnValue;
  }

  def executeWithRetry(retryCount: Int, op: => Int): Option[Int] = {
    for (i <- 1 to retryCount) {
      try {
        return Option(op)
      } catch {
        case ex: Exception => { }
      }
    }
    None;
  }

}
