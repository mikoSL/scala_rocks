object BeerSong {
def verse(number: Int): String = number match {
    case 2 =>
      "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n"
    case 1 =>
      "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n"
    case 0 =>
      "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
    case _ =>
      f"$number bottles of beer on the wall, $number bottles of beer.\nTake one down and pass it around, ${number - 1} bottles of beer on the wall.\n"
  }

  def verses(begin: Int, end: Int): String = {
    (begin to end by -1).map {verse(_)} .mkString("\n")
  }
}
