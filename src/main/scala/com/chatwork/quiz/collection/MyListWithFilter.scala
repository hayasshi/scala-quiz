package com.chatwork.quiz.collection

sealed trait MyListWithFilter[+A] {

  def withFilter(f: A => Boolean): MyListWithFilter[A] = MyNilWithFilter

  def map[B](f: A => B): MyList[B] = MyNil

  def flatMap[B](f: A => MyList[B]): MyList[B] = MyNil

}

case class MyConsWithFilter[A](mc: MyCons[A], mf: A => Boolean) extends MyListWithFilter[A] {

  override def withFilter(f: A => Boolean): MyListWithFilter[A] = MyConsWithFilter(mc, (x: A) => mf(x) && f(x))

  override def map[B](f: A => B): MyList[B] = {
    mc.foldRight(MyNil: MyList[B]) { (a, b) =>
      if (mf(a)) f(a) :: b
      else b
    }
  }

  override def flatMap[B](f: A => MyList[B]): MyList[B] = {
    mc.foldRight(MyNil: MyList[B]) { (a, b) =>
      if (mf(a)) f(a) ++ b
      else b
    }
  }

}

object MyNilWithFilter extends MyListWithFilter[Nothing]