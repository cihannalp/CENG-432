trait IyteImmutableList {
  def add(x: Int): IyteImmutableList

  def toString: String
}

case class Cons(h: Int, t: IyteImmutableList) extends IyteImmutableList {
  def add(x: Int): IyteImmutableList = Cons(x, Cons(h, t))

  override def toString: String = h + {
    t match {
      case c: Cons => "," + c.toString
      case Nil     => Nil.toString
    }
  }
}

case object Nil extends IyteImmutableList {
  def add(x: Int): IyteImmutableList = Cons(x, Nil)

  override def toString: String = ""
}

object IyteImmutableList {
  def apply(): IyteImmutableList = Nil

  def apply(x: Int): IyteImmutableList = Cons(x, Nil)
}
