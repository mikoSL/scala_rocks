object Bob {
  def isAllUppercase(question: String): Boolean = {
   val capitalLetters = 'A' to 'Z'
   val lowLetters = 'a' to 'z'
   val questionOnlyCapLetter = question.filter{x => capitalLetters.exists(_ == x)}
   val questionLowerLetter = question.filter{x => lowLetters.exists(_ == x)}

   var i = 0
   for (c <- questionOnlyCapLetter if c.isUpper) yield i += 1

   return (i == questionOnlyCapLetter.length + questionLowerLetter.length &&
   (questionOnlyCapLetter.length + questionLowerLetter.length) > 0)
   }

  def endWithQuestion(question: String): Boolean = {
   question.filterNot((x: Char) => x.isWhitespace).last == '?'
  }

  def response(statement: String): String = statement match {
   case silence if silence.trim.isEmpty  => "Fine. Be that way!"
   case question if endWithQuestion(question) && !isAllUppercase(question) => "Sure."
   case shouting if isAllUppercase(shouting) && !endWithQuestion(shouting) => "Whoa, chill out!"
   case angryQuestion if isAllUppercase(angryQuestion) && endWithQuestion(angryQuestion) => "Calm down, I know what I'm doing!"
   case _ => "Whatever."
  }
}
