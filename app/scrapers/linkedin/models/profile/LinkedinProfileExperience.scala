package scrapers.linkedin.models.profile

import common.HTMLMatcher
import org.apache.commons.lang3.StringEscapeUtils
import play.api.libs.json._

case class LinkedinProfileExperience(
  id: String,
  title: String,
  company: LinkedinCompany,
  start: String,
  end: String,
  duration: String,
  location: String,
  descriptionHTML: String)
object LinkedinProfileExperience {
  implicit val linkedinProfileExperienceFormat = Json.format[LinkedinProfileExperience]

  def create(content: String): Option[LinkedinProfileExperience] = {
    val id = getId(content).getOrElse("")
    val title = getTitle(content).getOrElse("")
    val company = getCompanyStr(content).flatMap(str => LinkedinCompany.create(str)).getOrElse(new LinkedinCompany("", "", ""))
    val start = getStart(content).getOrElse("")
    val end = getEnd(content).getOrElse("")
    val duration = getDuration(content).getOrElse("")
    val location = getLocation(content).getOrElse("")
    val descriptionHTML = getDescriptionHTML(content).getOrElse("")
    Some(new LinkedinProfileExperience(id, title, company, start, end, duration, location, descriptionHTML))
  }

  def getId(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<div id=\"experience-([0-9]*)-view\">")(0)
  def getTitle(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<h4>([^<]*)</h4>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getCompanyStr(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<h5>(.*?)</h5>")(0)
  def getStart(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<time>([^<]*)</time> &#8211; ")(0)
  def getEnd(content: String): Option[String] = HTMLMatcher.simple(content, "(?i) &#8211; <time>([^<]*)</time>")(0)
  def getDuration(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)\\(([^)]*)\\)<span class=\"locality\">")(0)
  def getLocation(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<span class=\"locality\">([^<]*)</span>")(0)
  def getDescriptionHTML(content: String): Option[String] = HTMLMatcher.simple(content, "(?is)<p class=\"description summary-field-show-more\">(.*?)</p>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
}
