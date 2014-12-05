package scrapers.linkedin.models.profile

import common.HTMLMatcher
import org.apache.commons.lang3.StringEscapeUtils
import play.api.libs.json._

case class LinkedinSchool(
  id: String,
  name: String,
  url: String)
object LinkedinSchool {
  implicit val linkedinSchoolFormat = Json.format[LinkedinSchool]

  def create(content: String): Option[LinkedinSchool] = {
    val idOpt = getId(content)
    val id = idOpt.getOrElse("")
    val name = getName(content).getOrElse("")
    val url = idOpt.map(id => "http://www.linkedin.com/edu/school?id=" + id).getOrElse("")
    Some(new LinkedinSchool(id, name, url))
  }

  def getId(content: String): Option[String] = getData(content)(0)
  def getName(content: String): Option[String] = getData(content)(1).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getData(content: String): List[Option[String]] = HTMLMatcher.simple(content, "(?i)(?:<a href=['\"](?:http://www.linkedin.com)?/edu/school\\?id=([0-9]+)&(?:amp;)?trk=ppro_sprof['\"] title=\"Plus de détails pour cette école\">)?([^<]*)(?:</a>)?")
}
