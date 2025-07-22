package fpinscala.exercises.parsing

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] =
    import P.*
    val spaces = char(' ').many.slice
    ???
