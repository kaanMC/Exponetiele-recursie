object Main extends App:
  sealed trait Pure[A]:
    def result: A = ???
    def map[B](f: A => B): Pure[B] = ???
    def flatMap[B](f: A => Pure[B]) : Pure[B]  = ???

  case class Done[A](a: A) extends Pure[A]
  

  def a1(m: Int, n: Int): Pure[Int] =
    if      (m == 0) Done(n+1)
    else if (n == 0) a1(m - 1, 1)
    else             for{
                        inner <- a1(m,n-1)
                        outer <- a1(m-1,inner)
                      } yield outer 

  def a2(m: Int, n: Int):  Pure[Int] =
    (m, n) match
      case (0, _) =>  Done(n+1)
      case (_, 0) => a2(m - 1, 1)
      case (_, _) => for{
                      inner <- a2(m,n-1)
                      outer <- a2(m-1,inner)
                    } yield outer 

  println(a1(3,3).result)
  println("test")
