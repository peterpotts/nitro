package com.peterpotts.nitro

import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

class JosephusTest extends WordSpec with Matchers {
  "Josephus object" should {
    "throw exception if n is invalid" in {
      intercept[IllegalArgumentException](Josephus(n = 0, k = 1))
    }

    "throw exception if k is invalid" in {
      intercept[IllegalArgumentException](Josephus(n = 1, k = 0))
    }

    "return 1 if n = 1 and k = 1" in {
      Josephus(n = 1, k = 1) shouldEqual 1
    }

    "return 3 if n = 3 and k = 2" in {
      Josephus(n = 3, k = 2) shouldEqual 3
    }

    "return 4 if n = 5 and k = 3" in {
      Josephus(n = 5, k = 3) shouldEqual 4
    }

    "return n if k = 1" in {
      val n = Random.nextInt(1000000) + 1
      Josephus(n = n, k = 1) shouldEqual n
    }

    "return 2(n - highestOneBit(n)) + 1 if k = 2" in {
      val n = Random.nextInt(1000000) + 1
      Josephus(n = n, k = 2) shouldEqual Josephus.kEqual2(n)
    }

    "return consistent value using naive and order n algorithm" in {
      val n = Random.nextInt(1000) + 1
      val k = Random.nextInt(1000) + 1
      Josephus(n = n, k = k) shouldEqual Josephus.orderN(n = n, k = k)
    }

    "return consistent value for n = k" in {
      val n = Random.nextInt(1000) + 1
      Josephus(n = n, k = n) shouldEqual Josephus.orderN(n = n, k = n)
    }
  }
}
