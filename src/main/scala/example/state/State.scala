package example.state

case class State[S,A](run: S => (A,S)) {
  import State._

  def map[B](f: A => B): State[S,B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S,B]): State[S,B] = State(
    s => {
      val (a,s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))

  def sequence[S,A](lsa: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, sts: List[State[S, A]], acc: List[A]): (List[A], S) = {
      sts match {
        case Nil => (acc.reverse, s)
        case h::t => h.run(s) match { case (a, s2) => go(s2, t, a :: acc) }
      }
    }
    State((s: S) => go(s, lsa, List()))
  }

  def get[S]: State[S,S] = State(s => (s,s))
  def set[S](s: S): State[S,Unit] = State( _ => ((), s))

  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield()
}
