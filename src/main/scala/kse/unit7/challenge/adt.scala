package kse.unit7.challenge

object adt:

  enum Try[+V]:

    case Success(value: V)             extends Try[V]
    case Failure(exception: Throwable) extends Try[Nothing]

    def flatMap[Q](f: V => Try[Q]): Try[Q] =
      this match
        case Try.Failure(e) => Try.Failure(e)
        case Try.Success(v) =>
          try f(v)
          catch case ex: Throwable => Try.Failure(ex)

    def map[Q](f: V => Q): Try[Q] =
      this match
        case Try.Failure(e) => Try.Failure(e)
        case Try.Success(v) =>
          try Try.Success(f(v))
          catch case ex: Throwable => Try.Failure(ex)

  object Try:

    def apply[V](v: V): Try[V] =
      if v == null then Try.Failure(new NullPointerException("Value cannot be null"))
      else Try.Success(v)
