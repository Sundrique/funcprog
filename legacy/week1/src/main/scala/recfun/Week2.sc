class Rational(x: Int, y: Int) {
  def numer = x

  def denum = y

  def add(that: Rational): Rational =
    new Rational(
      numer * that.denum + that.numer * denum,
      denum * that.denum
    )

  def neg() = new Rational(-numer, denum)

  def sub(that: Rational) = add(that.neg())

  override def toString = numer + "/" + denum
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.sub(y).sub(z)

if (true) 1 else new Rational(1, 2)

