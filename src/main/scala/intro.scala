/**
  * Created by bougadar on 07/07/17.
  */
import cats.functor
import javax.swing.Box

trait Printable[A] {
  self =>
  def format(value: A): String
  def contramap[B](func:B => A):Printable[B] = (value: B) => self.format(func(value))
}

def format[A](value:A)(implicit p : Printable[A]) : String = p.format(value)

object PrintableInstance {
  implicit val printableString = new Printable[String] {
    override def format(value: String): String = value
  }

  implicit val printableInt = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "non"


}


object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(p.format(value))


}


object PrintableSyntax {

  implicit class PrintOps[A](value: A) {
    def format(implicit p: Printable[A]): String = {
      p.format(value)
    }

    def print(implicit p: Printable[A]): Unit = {
      println(p.format(value))
    }
  }

}

final case class Box[A](value: A)

final case class Cat(name: String, age: Int, color: String)


implicit def boxPrintable[A](implicit p: Printable[A])=
  new Printable[Box[A]] {
    override def format(box: Box[A]): String = p.format(box.value)
  }

implicit def boxPrintable[A](implicit p : Printable[A])
                  = p.contramap[Box[A]](_.value)

object Cat {
  import PrintableInstance._

  implicit val catPrintable = new Printable[Cat] {
    override def format(value: Cat): String = {
      val name = Printable.format(value.name)
      val age = Printable.format(value.age)
      val color = Printable.format(value.color)
      s"$name is $age is year-old $color cat."
    }
  }
}


object test extends App {

  import PrintableSyntax._

 def garfield= Cat("Garfield", 35, "ginger-black")
  garfield.print


}
