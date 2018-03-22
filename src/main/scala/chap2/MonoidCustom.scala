package chap2

import cats.Monoid
import cats.instances.int._
import cats.syntax.semigroup._


trait Semigroup[A]{
  def combine(x:A,y:A):A
}


trait MonoidCustom[A] extends Semigroup[A] {
  def combine(x: A, y: A): A

  def empty: A

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: MonoidCustom[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: MonoidCustom[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

  implicit def SetUnionMonoid[A] : MonoidCustom[Set[A]] = new MonoidCustom[Set[A]]{
    override def combine(a: Set[A], b: Set[A]): Set[A] = a union b

    override def empty: Set[A] = Set.empty[A]
  }
}

object MonoidExercice {

  case class Order(totalCost: Double, quantity: Double)

  implicit val monoid:Monoid[Order] =new Monoid[Order]{
    def combine(o1:Order,o2:Order): Order ={
      Order(o1.totalCost + o2.totalCost,o1.quantity+o2.quantity)
    }

    override def empty: Order = Order(0,0)
  }


  def add[A](items: List[A])(implicit monoid: Monoid[A]) = {
    items.foldLeft(monoid.empty)(_ |+| _)

  }

}








