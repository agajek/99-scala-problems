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

  /**
    * P11 (*) Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
    Example:
      scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    */

  def encodeModified[A](list: List[A]): List[Any] =
    pack(list).map { l =>
      val length = l.length
      if(length == 1)
        l.head
      else
        (length, l.head)
    }

  /**
    * P12 (**) Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
    Example:
      scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
      res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    */
    def decode(list: List[(Int, Symbol)]): List[Symbol] = {
      def unpack(x: Int, s: Symbol) = {
        for(i <- 0 until x) yield s
      }

      list match {
        case Nil => Nil
        case (n, x) :: xs => unpack(n, x).toList ::: decode(xs)
      }
    }

  /**
    * P13 (**) Run-length encoding of a list (direct solution).
    Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
    Example:
      scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    */
  def encodeDirect(list: List[Symbol]): List[(Int, Symbol)] = {
    def f(last: Symbol, count: Int, rest: List[Symbol]): List[(Int, Symbol)] = rest match {
      case Nil => (count, last) :: Nil
      case x :: xs if x == last => f(x, count + 1, xs)
      case x :: xs => (count, last) :: f(x, 1, xs)
    }

    list match {
      case Nil => Nil
      case x :: xs => f(x, 1, xs)
    }
  }

  /**
    * P14 (*) Duplicate the elements of a list.
    Example:
      scala> duplicate(List('a, 'b, 'c, 'c, 'd))
      res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    */
  def duplicate(list: List[Symbol]): List[Symbol] =
    list.flatMap(s => List(s,s))

  /**
    * P15 (**) Duplicate the elements of a list a given number of times.
    Example:
      scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
      res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    */
  def duplicateN(count: Int, list: List[Symbol]): List[Symbol] =
    list.flatMap(s => List.fill(count)(s))

  /**
    * P16 (**) Drop every Nth element from a list.
    Example:
      scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  */
  def drop(count: Int, list: List[Symbol]): List[Symbol] = {
    def f(c: Int, l: List[Symbol]): List[Symbol] = l match {
      case Nil => Nil
      case x :: xs if c == count => f(1, xs)
      case x :: xs => x :: f(c + 1, xs)
    }

    f(1, list)
  }

  /**
    * P17 (*) Split a list into two parts.
    The length of the first part is given. Use a Tuple for your result.
    Example:
      scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  */
  def split(where: Int, list: List[Symbol]): (List[Symbol], List[Symbol]) = {
    def f(c: Int, tail: List[Symbol], head: List[Symbol]): (List[Symbol], List[Symbol]) = tail match {
      case Nil => (list, Nil)
      case x :: xs if c == where => (head :+ x , xs)
      case x :: xs => f(c + 1, xs, head :+ x)
    }

    f(1, list, Nil)
  }

  /**
    * P18 (**) Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
    Example:
      scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: List[Symbol] = List('d, 'e, 'f, 'g)
  */
  def slice(from: Int, to: Int, list: List[Symbol]): List[Symbol] = {
    def f(c: Int, acc: List[Symbol])(tail: List[Symbol]): List[Symbol] = tail match {
      case Nil => acc
      case x :: xs if c >= from && c < to => f(c+1, x :: acc)(xs)
      case x :: xs if c > to => acc
      case x :: xs  => f(c+1, acc)(xs)
    }

    f(0, Nil)(list).reverse
  }

  /**
    * P19 (**) Rotate a list N places to the left.
    Examples:
      scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

      scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  */
  def rotate(where: Int, list: List[Symbol]): List[Symbol] = {
    val length = list.length
    val split = (length + where) % length

    def f(c: Int, acc: List[Symbol])(tail: List[Symbol]): List[Symbol] = tail match {
      case Nil => Nil
      case xs if c >= split => xs ::: acc
      case x :: xs => f(c+1, acc :+ x)(xs)
    }

    f(0, Nil)(list)
  }

  /**
    * P20 (*) Remove the Kth element from a list.
    Return the list and the removed element in a Tuple. Elements are numbered from 0.
    Example:
      scala> removeAt(1, List('a, 'b, 'c, 'd))
      res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  */
  def removeAt(where: Int, list: List[Symbol]): (List[Symbol], Symbol) = {
    def f(c: Int, acc: List[Symbol])(tail: List[Symbol]): (List[Symbol], Symbol) = tail match {
      case x :: Nil => (Nil, x)
      case x :: xs if c == where => (acc ::: xs, x)
      case x :: xs => f(c+1, acc :+ x)(xs)
    }

    f(0, Nil)(list)
  }

  /**
    * P21 (*) Insert an element at a given position into a list.
    Example:
      scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
      res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  */
  def insertAt(e: Symbol, where: Int, list: List[Symbol]): List[Symbol] = list match {
    case Nil => e :: Nil
    case x :: xs if where == 1 => x :: e :: xs
    case x :: xs => x :: insertAt(e, where - 1, xs)
  }

  /**
    * P22 (*) Create a list containing all integers within a given range.
    Example:
      scala> range(4, 9)
      res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  */
  def range(from: Int, to: Int): List[Int] =
    if(from == to) to :: Nil
    else from :: range(from + 1, to)


}
