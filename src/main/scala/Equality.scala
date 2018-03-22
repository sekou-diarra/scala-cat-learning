import java.util.Date

import cats.Eq
import cats.instances.int._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._
import cats.instances.long._

val eqInt = Eq[Int]

eqInt.eqv(123,123)

123 === 123
123 =!= 1234


(Some(1):Option[Int]) === (None:Option[Int])
Option(1) === Option.empty[Int]

1.some === None
1.some =!= None

implicit val dateEquals: Eq[Date] =Eq.instance[Date]{ (date1, date2)=>
  date1.getTime === date2.getTime
}

val x = new Date()
val y = new Date()

final case class Cat(name:String, age:Int, color:String)

object Cat{
 import cats.instances.int._
 import cats.instances.string._
 import cats.syntax.eq._

  implicit def catEqual= Eq.instance[Cat]{(cat1,cat2)=>
    (cat1.name === cat2.name) && (cat1.age === cat2.age) && (cat1.color === cat2.color)
  }
}

val cat1 = Cat("Garfield", 35, "orange and black")
val cat2 = Cat("Heathcliff", 30, "orange and black")

cat1 === cat2

