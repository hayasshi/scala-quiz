package com.chatwork.quiz.collection

import com.chatwork.quiz.{ MyNone, MyOption }
import org.omg.CORBA.Current

import scala.annotation.tailrec

sealed trait MyList[+A] {

  // Easy
  def length: Int = foldLeft(0)((b, _) => b + 1)

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(b: B, next: MyList[A]): B = {
      next match {
        case MyCons(head, tail) => go(f(b, head), tail)
        case MyNil              => b
      }
    }
    go(z, this)
  }

  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B = foldLeft((b: B) => b)((b, a) => b.compose(f(a, _)))(z)

  // Normal
  def ::[B >: A](b: B): MyList[B] = MyCons(b, this)

  // Normal
  def reverse: MyList[A] = foldLeft(MyNil: MyList[A])((b, a) => MyCons(a, b))

  // Normal
  def ++[B >: A](b: MyList[B]): MyList[B] = foldRight(b) { _ :: _ }

  // Normal
  def map[B](f: A => B): MyList[B] = foldRight(MyNil: MyList[B])((a, b) => f(a) :: b)

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] = foldRight(MyNil: MyList[B])((a, b) => f(a) ++ b)

  // Normal
  def filter(f: A => Boolean): MyList[A] = foldRight(MyNil: MyList[A])((a, b) => if (f(a)) MyCons(a, b) else b)

  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyListWithFilter[A] = {
    this match {
      case x: MyCons[A] => MyConsWithFilter(x, f)
      case MyNil        => MyNilWithFilter
    }
  }

  // Normal
  def find(f: A => Boolean): MyOption[A] = {
    filter(f) match {
      case MyCons(head, _) => MyOption(head)
      case MyNil           => MyNone
    }
  }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = {
    @tailrec
    def go(a: MyList[A], b: MyList[B]): Boolean = {
      (a, b) match {
        case (MyCons(h1, t1), MyCons(h2, t2)) => if (h1 == h2) go(t1, t2) else false
        case (MyCons(_, _), MyNil)            => true
        case (MyNil, MyCons(_, _))            => false
        case (MyNil, MyNil)                   => true
      }
    }
    go(this, prefix)
  }

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] = as.foldRight(MyNil: MyList[A])((a, b) => MyCons(a, b))

}
