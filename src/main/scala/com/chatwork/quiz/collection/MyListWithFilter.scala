package com.chatwork.quiz.collection

sealed trait MyListWithFilter[+A] {

  def withFilter(f: A => Boolean): MyListWithFilter[A] = this match {
    case MyConsWithFilter(myList, myFilter) => MyConsWithFilter(myList, (a: A) => myFilter(a) && f(a))
    case MyNilWithFilter                    => MyNilWithFilter
  }

  def map[B](f: A => B): MyList[B] = this match {
    case MyConsWithFilter(myList, myFilter) => myList.foldRight(MyNil: MyList[B])((a, b) => if (myFilter(a)) { f(a) :: b } else { b })
    case MyNilWithFilter                    => MyNil
  }

  def flatMap[B](f: A => MyList[B]): MyList[B] = this match {
    case MyConsWithFilter(myList, myFilter) => myList.foldRight(MyNil: MyList[B])((a, b) => if (myFilter(a)) { f(a) ++ b } else { b })
    case MyNilWithFilter                    => MyNil
  }

}

case class MyConsWithFilter[A](myList: MyList[A], myFilter: A => Boolean) extends MyListWithFilter[A]

object MyNilWithFilter extends MyListWithFilter[Nothing]
