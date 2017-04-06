def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
  if (cond) onTrue() else onFalse()

if2(true, () => "blah", () => "no blah")

def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if (cond) onTrue else onFalse

if3(true, "bkah", "blue")



val a = Stream(1,2,3)

a.append(Stream(3,4,5,6)).toList

val ones: Stream[Int] = cons(1, ones)
val twos = ones.map(_ + 1)

val as: Stream[String] = cons("A", as)
val fiveAs = as.take(5)

fiveAs.toList

val ints: Stream[Int] = from(Int.MaxValue)

val firstFiveInts = ints.take(5).toList

val fibSeries = ones
fibSeries.take(10).toList


val x = Stream(1,2,3,4,5)