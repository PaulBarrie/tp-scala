package org.esgi

import scala.annotation.tailrec

object Main {
  
  /*
    Exercice 1
   */
  // Question 1 
  def renverser(chaine: String): String = {
    chaine match {
      case "" => ""
      case _ => renverser(chaine.tail) + chaine.head
    }
  }

  // Question 2
  def palindrome(chaine: String): Boolean = {
    chaine match {
      case "" => true
      case _ => if (chaine.head == chaine.last) palindrome(chaine.tail.init) else false
    }
  }
  
  //Question 3
  def reprBinaire(n: Long): String = {
    @tailrec
    def helper(n: Long, acc: String): String = {
      if (n == 0) acc
      else helper(n / 2, (n % 2).toString + acc)
    }
    helper(n, "")
  }
  
  /*
    Exercice 2
   */
  sealed trait Article

  final case class Regular(name: String, category: String, price: Double) extends Article

  // discount est compris entre 0 et 1
  final case class Discounted(name: String, category: String, price: Double, discount: Float) extends Article
  // Question 1
  def applyDiscount(article: Discounted): Double = {
    article.price * (1 - article.discount)
  }

  // Question 2
  def price(article: Article): Double = {
    article match {
      case Regular(_, _, price) => price
      case Discounted(_, _, _, _) => applyDiscount(article.asInstanceOf[Discounted])
    }
  }
  
  // Question 3
  def cartAmount(articles: List[Article]): Double = {
    articles match {
      case Nil => 0
      case head :: tail => price(head) + cartAmount(tail)
    }
  }
  // -> prix total du panier : 180.29099744319916

  //Question 4
  def applyCoupon(coupon: Float, category: String): Function[Article, Double] = {
    article => {
      article match {
        case Regular(_, cat, price) => if (cat == category) (1 - coupon ) * price else price
        case Discounted(_, cat, _, _) => if (cat == category) applyDiscount(article.asInstanceOf[Discounted]) * (1 - coupon) else applyDiscount(article.asInstanceOf[Discounted])
      }
    }
  }
  
  // Question 5
  def cartAmountWithCoupon(articles: List[Article], coupon: Article => Double): Double = {
    articles match {
      case Nil => 0
      case head :: tail => coupon(head) + cartAmountWithCoupon(tail, coupon)
    }
  }
  // -> prix total avec coupon sur food : 179.29099744319916
  
  /*
    Exercice 3
   */

  sealed trait Liste[+A] {

    // Question 2
    def map[B](f: A => B): Liste[B] = {
      this match {
        case NonVide(tete, queue) => NonVide(f(tete), queue.map(f))
        case Vide => Vide
      }
    }
    // Question 3
    private def concat(other: Liste[A]): Unit = {
      this match {
        case NonVide(tete, queue) => {
          queue.concat(other)
          NonVide(tete, queue)
        }
        case Vide => other
      }
    }
    def flatMap[B](f: A => Liste[B]): Liste[B] = {
      Liste
      this match {
        case NonVide(tete, queue) => {
          val l = f(tete)
          l.concat(queue.flatMap(f))
          l
        }
        case Vide => Vide
      }
    } 
    // Question 4
    def fold[B >: A](default: B, f: (B, B) => B): B = {
      this match {
        case NonVide(tete, queue) => f(tete, queue.fold(default, f))
        case Vide => default
      }
    }
  }
  case class NonVide[+A](tete: A, queue: Liste[A]) extends Liste[A] 
  object Vide extends Liste[Nothing]
  
  // Question 1
  object Liste {
    def apply[A](first: A, others: A*): Liste[A] = {
      val othersList = others.toList
      othersList match {
        case Nil => NonVide(first, Vide)
        case head :: tail => NonVide(first, apply(head, tail: _*))
      }
    }
  }
  
  def main(args: Array[String]) {
    // Exercice 1
    //println(renverser("!"))
    //println(palindrome("kayak"))
    //println(reprBinaire(6))
    
    // Exercice 2
    val articles: List[Article] = List(
      Regular(name = "Biscuits", category = "food", price = 2.0),
      Discounted(name = "Monitor", category = "tech", price = 119.99, discount = 0.1f),
      Discounted(name = "Mouse", category = "tech", price = 25.50, discount = 0.2f),
      Regular(name = "dress", category = "clothes", price = 49.90)
    )
    
    
    println(cartAmount(articles))
    println(cartAmountWithCoupon(articles, applyCoupon(0.5f, "food")))
    
    // Exercice 3
    // Question 1
    println(Liste(1, 2, 3, 4, 5, 6) == NonVide(1, NonVide(2, NonVide(3, NonVide(4, NonVide(5, NonVide(6, Vide)))))))
    println(Liste(1) == NonVide(1, Vide))
    println(Liste(1, 2, 3) == NonVide(1, NonVide(2, NonVide(3, Vide))))
    
    // Question 2
    println((Vide: Liste[Int]).map[Int](x => x + 1) ==  Vide)
    println(Liste(1, 2, 3, 4).map(x => x + 1) == Liste(2, 3, 4, 5))
    
    // Question 3
    println((Vide: Liste[Int]).flatMap[Int](x => Liste(x + 1)) == Vide)
    println(Liste(1, 2, 3, 4).flatMap(x => Liste(x + 1)) == Liste(2, 3, 4, 5))

    // Question 4
    println((Vide: Liste[Int]).fold(0, (a, b) => a + b) == 0)

    println(Liste(1, 2, 3, 4).fold(0, (a, b) => a + b) == 10)
    
  }

}
