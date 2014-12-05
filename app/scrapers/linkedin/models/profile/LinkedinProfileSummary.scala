package scrapers.linkedin.models.profile

import common.HTMLMatcher
import play.api.libs.json._

case class LinkedinProfileSummary(
  currentActivities: List[LinkedinCompany],
  pastActivities: List[LinkedinCompany],
  education: List[LinkedinSchool],
  websites: List[LinkedinWebsite])
object LinkedinProfileSummary {
  implicit val linkedinProfileSummaryFormat = Json.format[LinkedinProfileSummary]

  def create(content: String): Option[LinkedinProfileSummary] = {
    val currentActivities = getItemsFor(content, "overview-summary-current").map(_.map(str => LinkedinCompany.create(str)).flatten).getOrElse(List())
    val pastActivities = getItemsFor(content, "overview-summary-past").map(_.map(str => LinkedinCompany.create(str)).flatten).getOrElse(List())
    val education = getItemsFor(content, "overview-summary-education").map(_.map(str => LinkedinSchool.create(str)).flatten).getOrElse(List())
    val websites = getItemsFor(content, "overview-summary-websites").map(_.map(str => LinkedinWebsite.create(str)).flatten).getOrElse(List())
    Some(new LinkedinProfileSummary(currentActivities, pastActivities, education, websites))
  }

  def getItemsFor(content: String, id: String): Option[List[String]] = HTMLMatcher.simple(content, "(?i)<tr id=\"" + id + "\">(.*?)</tr>")(0).map(str => HTMLMatcher.multi(str, "(?i)<li>(.*?)</li>").map(item => item(0)).flatten)
}
