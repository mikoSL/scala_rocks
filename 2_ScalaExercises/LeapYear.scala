object Leap {
  def isDividedBy(year: Int, num: Int): Boolean = year % num == 0

  def leapYear(year: Int): Boolean = {
    (isDividedBy(year, 4) && !isDividedBy(year, 100)) || isDividedBy(year, 400)
 }
}
