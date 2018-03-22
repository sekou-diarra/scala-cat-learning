import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

val showInt:Show[Int]= Show.apply[Int]
val showString:Show[String]= Show.apply[String]

val intAsString:String=showInt.show(23)
val stringAsString:String= showString.show("abd")

// with interface syntax
val showIntSyntax=123.show
val showStringSyntax="abc".show

final case class CatShow(name: String, age: Int, color: String)

//reimplementation of exo 1 with cats type classes
object CatShow {
  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  implicit val catShow = Show.show[CatShow]{cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }
}


