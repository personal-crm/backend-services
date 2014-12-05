package scrapers.linkedin.models.profile

import common.HTMLMatcher
import org.apache.commons.lang3.StringEscapeUtils
import play.api.libs.json._

case class LinkedinRelatedProfile(
  name: String,
  headline: String,
  image: String,
  link: String)
object LinkedinRelatedProfile {
  implicit val linkedinRelatedProfileFormat = Json.format[LinkedinRelatedProfile]

  def create(content: String): Option[LinkedinRelatedProfile] = {
    val name = getName(content).getOrElse("")
    val headline = getHeadline(content).getOrElse("")
    val image = getImage(content).getOrElse("")
    val link = getLink(content).getOrElse("")
    Some(new LinkedinRelatedProfile(name, headline, image, link))
  }

  def getImage(content: String): Option[String] = getData(content)(0)
  def getLink(content: String): Option[String] = getData(content)(1)
  def getName(content: String): Option[String] = getData(content)(2)
  def getHeadline(content: String): Option[String] = getData(content)(3).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getData(content: String): List[Option[String]] = HTMLMatcher.simple(content, "(?i)<a class=\"browse-map-photo\" title=\"[^\"]+\" href='[^']+'><img class=\"\" alt=\"[^\"]+\" width=\"60\" height=\"60\" src=\"([^\"]+)\"></a><h4><a href=\"([^\"]+)\">([^<]+)</a></h4><p class=\"browse-map-title\">([^<]+)</p>")
}
