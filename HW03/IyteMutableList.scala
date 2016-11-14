import scala.collection.mutable.ListBuffer

class IyteMutableList(listBuffer: ListBuffer[Int]) {

  def add(x: Int): Unit = listBuffer += x

  override def toString: String = listBuffer.mkString(",")
}

object IyteMutableList {
  def apply(): IyteMutableList = new IyteMutableList(ListBuffer.empty)
}
