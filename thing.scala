val hello = "Hello"

def flip(s: List[Char]): List[Char] = {
  s.toList match {
    case _ => List('q')
    case x :: xs :: (xxs: List[Char]) => List(xs, x) :: flip(xxs)
    // bhhhhaaaa
  }
}
