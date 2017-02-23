/** Exercise solutions for the book Functional Programming in Scala*/

// Calculate factorial, 
def factorial(n: Int): BigInt = {
  @annotation.tailrec
  def go(n: Int, acc: Int): BigInt = {
    if (n <= 0) acc
    else go(n - 1, n * acc)
  }
  go(n, 1)
}


// Return n-th Fibonacci number w/ tail call optimization
def fib(n: Int): BigInt = {
  @annotation.tailrec
  def go(a: BigInt, b: BigInt, n: Int): BigInt = {
    if (n == 1) b
    else if (n == 0) a
    else go(b, a + b, n - 1)
  }
  go(0, 1, n)
}


// Bisection search, assumes sorted input
def bisect(array: Array[Int], search: Int): Int = {
  @annotation.tailrec
  def go(low: Int, high: Int): Int = {
    val mid = (low + high) / 2
    if (array(mid) == search) mid
    else if (search < array(low)) -1
    else if (search < array(mid)) go(low, mid)
    else go(mid + 1, high)
  }
  go(0, array.length - 1)
}


// Checks if an array is sorted
def isSorted[A](array: Array[A], le: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(left: Int, right: Int): Boolean = {
    if (right == (array.length - 1)) {
      if (le(array(left), array(right))) true
      else false
    }
    else if (!le(array(left), array(right))) false
    else go(right, right + 1)
  }
  if (array.length == 1 | array.length == 0) true
  else go(0, 1)
}


// Try to implement shorter version of isSorted
def isSorted[A](array: Array[A], le: (A, A) => Boolean): Boolean =
  array.zip(array.slice(1, array.length))
       .map(x => le(x._1, x._2))
       .reduce(_ & _)


// Partial application
def partial1[A,B,C](a: A, f: (A,B) => C): B => C = ((b) => f(a, b))


// Currying from a multiple arg function to a function that returns a function
def curry[A,B,C](f: (A, B) => C): A => (B => C) = ((a) => ((b) => f(a, b)))


// Uncurrying from a curried function to original with multiple args
def uncurry[A,B,C](f: A => B => C): (A, B) => C = ((a, b) => f(a)(b))

// Function composition f(g(a))
def compose[A,B,C](f: B => C, g: A => B): A => C = ((a) => f(g(a)))
