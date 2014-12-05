package scrapers.linkedin.models.profile

import common.HTMLMatcher
import java.net.URLDecoder
import play.api.libs.json._

case class LinkedinWebsite(
  name: String,
  url: String)
object LinkedinWebsite {
  implicit val linkedinWebsiteFormat = Json.format[LinkedinWebsite]

  def create(content: String): Option[LinkedinWebsite] = {
    val name = getName(content).getOrElse("")
    val url = getUrl(content).getOrElse("")
    Some(new LinkedinWebsite(name, url))
  }

  def getUrl(content: String): Option[String] = getData(content)(0).map(str => URLDecoder.decode(str, "UTF-8"))
  def getName(content: String): Option[String] = getData(content)(1)
  def getData(content: String): List[Option[String]] = HTMLMatcher.simple(content, "(?i)<a target=\"_blank\" href=\"(?:http://www.linkedin.com)?/redir/redirect\\?url=([^&]*)&urlhash=[^&]*&trk=ppro_website\">([^<]*)</a>")
}
