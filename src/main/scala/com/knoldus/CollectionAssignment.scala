package com.knoldus

class CollectionAssignment {

  //find sum and multiplication of the list
  def sumOfListElements(list: List[Int]): Int = {
    list.foldLeft(0) { (acc: Int, element: Int) => acc + element
    }
  }

  def productOfListElements(list: List[Int]): Long = {
    list.foldLeft(1) { (acc: Int, element: Int) => acc * element
    }
  }

  //print the table of each element in the List
  def printTableOfListElements(list: List[Int]): List[(Int, List[Int])] = {
    for {
      l <- list
      table = l -> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).map(_ * l)
    } yield table
  }

  // Find the last element of list with its index value
  def lastElementOfList(list: List[Int], indexCounter: Int): Option[(Int, Int)] = list match {
    case Nil => None
    case first :: Nil => Some(first, indexCounter)
    case first :: rest => lastElementOfList(rest, indexCounter + 1)
  }

}

//Implement Stack and Queue using Lists.
class StackImpl {
  def pop(list: List[String]): String = if (list.length > 1) list.head else "Stack Underflow"

  def push(element: String, list: List[String]): List[String] = if (list.length > 1) element :: list else List(element)

}

class QueueImpl {
  def deque(list: List[String]): Option[List[String]] = if (list.length > 1) Some(list.tail) else None

  def enqueue(element: String, list: List[String]): List[String] = if (list.length > 1) list :+ element else List(element)

}

object CollectionAssignmentOb extends App {

  val collectionAssignment = new CollectionAssignment

  val list = List(1, 2, 3, 4, 5)

  println(collectionAssignment.sumOfListElements(list))
  println(collectionAssignment.productOfListElements(list))

  val color = List("red", "blue", "green")
  val stack = new StackImpl
  println(stack.push("pink", color))
  println(stack.pop(color))

  val queue = new QueueImpl
  println(queue.enqueue("orange", color))
  println(queue.deque(color))

  val list1 = List(8, 9)
  println(collectionAssignment.printTableOfListElements(list1))

  println(collectionAssignment.lastElementOfList(list1, 0))
}
