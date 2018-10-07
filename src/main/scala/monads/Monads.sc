import scala.language.higherKinds
import cats.Monad
import cats.instances.option._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.Id

//trait Monad[F[_]]{
//  def pure[A](value:A):F[A]
//
//  def flatMap[A,B](value:F[A])(func:A=>F[B]):F[B]
//
//  def map[A,B](value: F[A])(func:A=>B):F[B] ={
//    flatMap(value)(a=>pure(func(a)))
//  }
//}

val opt1 = Monad[Option].pure(3)
val opt2 = Monad[Option].flatMap(opt1)(a => Some(a+2))
val opt3 = Monad[Option].map(opt2)(a =>100*a)


def sumSquare[F[_]: Monad](a:F[Int], b:F[Int]):F[Int]=
  a.flatMap(x=>b.map(y=>x*x + y*y))


sumSquare(Option(3),Option(4))
