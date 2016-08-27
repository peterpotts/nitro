package com.peterpotts.nitro

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Josephus {
  def main(args: Array[String]): Unit = {
    val Array(n, k) = args.map(_.toInt)
    println(apply(n = n, k = k))
  }

  /**
    * Naive implementation.
    */
  def apply(n: Int, k: Int): Int = {
    require(n > 0 && k > 0)

    @tailrec def loop(position: Int, queue: Queue[Int]): Int = {
      val (head, tail) = queue.dequeue

      if (tail.isEmpty) {
        head
      } else if (position == k) {
        loop(1, tail)
      } else {
        loop(position + 1, tail.enqueue(head))
      }
    }

    loop(1, Queue.empty ++ (1 to n))
  }

  /**
    * Optimized implementation for k = 2.
    */
  def kEqual2(n: Int): Int = {
    require(n > 0)
    2 * (n - Integer.highestOneBit(n)) + 1
  }

  /**
    * Optimized general implementation.
    */
  def orderN(n: Int, k: Int): Int = {
    require(n > 0 && k > 0)
    def loop(g: Int, n: Int) = (g + k) % n
    val gN = (2 to n).foldLeft(0)(loop)
    gN + 1
  }
}
