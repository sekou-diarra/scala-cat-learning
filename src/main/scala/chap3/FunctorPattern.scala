package chap3

import cats.Functor

import sun.reflect.generics.tree.Tree

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value:A) extends Tree[A]



object FunctorTree {



  implicit val treeFunctor:Functor[Tree] = new Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match {
        case Branch(left,right) =>
          Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
  }


}

object Tree extends App{
  import FunctorTree._


  def branch[A](left: Tree[A], right:Tree[A]): Tree[A] = Branch(left,right)

  def leaf[A](value: A) : Tree[A] = Leaf(value)

  Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_*2)
}
