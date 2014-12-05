package scrapers.linkedin.models.profile

import common.HTMLMatcher
import play.api.libs.json._

case class LinkedinCompany(
  id: String,
  name: String,
  url: String)
object LinkedinCompany {
  implicit val linkedinCompanyFormat = Json.format[LinkedinCompany]

  def create(content: String): Option[LinkedinCompany] = {
    val idOpt = getId(content)
    val id = idOpt.getOrElse("")
    val name = getName(content).getOrElse("")
    val url = idOpt.map(id => "http://www.linkedin.com/company/" + id).getOrElse("")
    Some(new LinkedinCompany(id, name, url))
  }

  def getId(content: String): Option[String] = getData(content)(0)
  def getName(content: String): Option[String] = getData(content)(1)
  def getData(content: String): List[Option[String]] =
    HTMLMatcher.simple(content, "(?i)<(?:a|span)(?: href=\"(?:http://www.linkedin.com)?/company/([0-9]+)\\?trk=ppro_cprof\")? dir=\"auto\">([^<]*)</(?:a|span)>")

  // content: <span dir=\"auto\">Entreprise individuelle</span>,
  // content: <a href=\"/company/732914?trk=ppro_cprof\" dir=\"auto\">Amundi</a>
  // content: <a href=\"http://www.linkedin.com/company/732914?trk=ppro_cprof\" dir=\"auto\">Amundi</a>
}
