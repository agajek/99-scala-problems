package lists

object Lists {

  /**
  P01 (*) Find the last element of a list.
  Example:
    scala> last(List(1, 1, 2, 3, 5, 8))
  res0: Int = 8
   */
  def last[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
  }


  /**
    * P02 (*) Find the last but one element of a list.
  Example:
    scala> penultimate(List(1, 1, 2, 3, 5, 8))
    res0: Int = 5
  */

  def penultimate[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: Nil => None
    case x :: y :: Nil => Some(x)
    case x :: xs => penultimate(xs)
  }

  /**
    * P03 (*) Find the Kth element of a list.
    By convention, the first element in the list is element 0.
    Example:
      scala> nth(2, List(1, 1, 2, 3, 5, 8))
      res0: Int = 2
  */

  def nth[A](n: Int, list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: xs if n == 0 => Some(x)
    case x :: xs => nth(n-1, xs)
  }

  /**
    * P04 (*) Find the number of elements of a list.
    Example:
      scala> length(List(1, 1, 2, 3, 5, 8))
      res0: Int = 6
  */

  def length[A](list: List[A]): Int = {
    def f(count: Int, l: List[A]): Int = l match {
      case Nil => count
      case x :: xs => f(count + 1, xs)
    }

    f(0, list)
  }

  /**
    * P05 (*) Reverse a list.
    Example:
      scala> reverse(List(1, 1, 2, 3, 5, 8))
      res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  */
  def reverse[A](list: List[A]): List[A] = {
    def f(acc: List[A], list: List[A]): List[A] = list match {
      case Nil => acc
      case x :: xs => f(x :: acc, xs)
    }

    f(Nil, list)
  }

  /**
    * P06 (*) Find out whether a list is a palindrome.
    Example:
      scala> isPalindrome(List(1, 2, 3, 2, 1))
      res0: Boolean = true
  */
  def isPalindrome[A](list: List[A]): Boolean =
    list == reverse(list)

  /**
    * P07 (**) Flatten a nested list structure.
  Example:
    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  */
  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case x :: xs if x.isInstanceOf[List[_]] => flatten(x.asInstanceOf[List[_]]) ++ flatten(xs)
    case x :: xs  => x :: flatten(xs)
  }

  /**
    * P08 (**) Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
    Example:
      scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
    */

  def compress[A](list: List[A]): List[A] = {
    def f(last: A, rest: List[A]): List[A] = rest match {
      case Nil => Nil
      case x :: xs if x == last => f(x, xs)
      case x :: xs => x :: f(x, xs)
    }

    list match {
      case Nil => Nil
      case x :: xs => x :: f(x, xs)
    }
  }

  /**
    * P09 (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.
    Example:
      scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  */
  def pack[A](list: List[A]): List[List[A]] = {
    def f(last: A, acc: List[A], rest: List[A]): List[List[A]] = rest match {
      case Nil => List(acc)
      case x :: xs if x == last => f(x, x :: acc, xs)
      case x :: xs => List(acc) ::: f(x, List(x), xs)
    }

    list match {
      case Nil => Nil
      case x :: xs => f(x, List(x), xs)
    }
  }

  /**
    * P10 (*) Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
    Example:
      scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  */
  def encode[A](list: List[A]): List[(Int, A)] =
    pack(list).map(l => (l.length, l.head))

}
