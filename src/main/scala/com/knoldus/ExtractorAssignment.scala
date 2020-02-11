package com.knoldus

class ExtractorAssignment

object ExtractorAssignmentOb extends App {

  val CustomTimeStamp(day) = " 01-01-2020 12:02:123"
  println(day)

  URL("https", "aws.amazon.com", "/console/home", Map("state" -> "hash", "isauthcode" -> "true", "code" -> "112"))

  val URL(protocol, domain1, path, params) = "https://aws.amazon.com/console/home?state=hash&isauthcode=true&code=112"
  println(s"$protocol\n$domain1\n$path\n$params")

  val ValidateEmail(username, domain2) = "suraj.saini@knoldus.com"
  println(s"$username\n$domain2")
}

object CustomTimeStamp {
  def unapply(timeStamp: String): Option[String] = {
    val day = timeStamp.trim.substring(0, 2)
    val validDayRegularExpression = """[0-9]{2}""".r
    if (validDayRegularExpression.matches(day)) Some(day) else None
  }
}

object ValidateEmail {
  val EMAIL = """^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)$""".r

  def unapply(email: String): Option[(String, String)] = {
    if (EMAIL.matches(email)) Some(email.split('@')(0), email.split('@')(1)) else None
  }
}

object URL {
  def apply(protocol: String, domain: String, path: String, params: Map[String, String]): String = {
    s"$protocol://$domain$path?" + params.map { case (key, value) => s"$key=$value" }.mkString("&")
  }

  //write if-else
  def unapply(url: String): Option[(String, String, String, Map[String, String])] = {

    val urlParts = url split '?'
    if (urlParts.length == 2) {
      val protocol: String = urlParts(0).split("://")(0)
      val domain: String = urlParts(0).split("://")(1).split("/")(0)
      val path: String = urlParts(0).split("://")(1).split("com")(1)
      val str: String = urlParts(1)
      val list = str split "&"
      val list1 = for {
        l <- list
      } yield (l.split("=")(0), l.split("=")(1))
      val params = list1.toMap
      Some(protocol, domain, path, params)
    } else {
      None
    }
  }
}
