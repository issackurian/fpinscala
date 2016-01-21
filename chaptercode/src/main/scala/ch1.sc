
case class Coffee() {
  def price = 10.0
}

case class CreditCard(number: Number, holderName: String)

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges from different cards")
  }
}

class Cafe {
  def buyCoffe(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffe(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
}

def coalesce(charges: List[Charge]): List[Charge] =
  charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList

val cc1 = CreditCard(1234, "shaji")
val cc2 = CreditCard(4321, "kailas")

val a = List(Charge(cc1, 10.0), Charge(cc1, 12.0), Charge(cc2, 5.0),
  Charge(cc2, 15.0), Charge(cc1, 17.0))

a.groupBy(_.cc)
a.groupBy(_.cc).values

coalesce(a)
