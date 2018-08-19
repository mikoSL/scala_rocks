/* Bob is a lackadaisical teenager. In conversation, his responses are very limited.

Bob answers 'Sure.' if you ask him a question.

He answers 'Whoa, chill out!' if you yell at him.

He answers 'Calm down, I know what I'm doing!' if you yell a question at him.

He says 'Fine. Be that way!' if you address him without actually saying anything.

He answers 'Whatever.' to anything else.
*/

object Bob {

  def response(statement: String): String = statement.trim match {
   case silence if silence.isEmpty  => "Fine. Be that way!"
   case question if question.endsWith("?") && !isAllUppercase(question) => "Sure."
   case shouting if isAllUppercase(shouting) => {
    if (shouting.endsWith("?"))  "Calm down, I know what I'm doing!"
    else "Whoa, chill out!"
    }
   case _ => "Whatever."
  }

  def isAllUppercase(question: String): Boolean = {
   question.exists(_.isLetter) && question.toUpperCase == question
   }
}
