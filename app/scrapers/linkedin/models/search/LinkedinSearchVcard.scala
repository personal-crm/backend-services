package scrapers.linkedin.models.search

import scrapers.linkedin.models.LinkedinProfile
import common.HTMLMatcher
import org.apache.commons.lang3.StringEscapeUtils
import play.api.libs.json._

case class LinkedinSearchVcard(
  image: String,
  firstName: String,
  lastName: String,
  fullName: String,
  headline: String,
  location: String,
  industry: String,
  currentActivities: String,
  pastActivities: String,
  education: String,
  resume: String,
  url: String)
object LinkedinSearchVcard {
  implicit val linkedinSearchVcardFormat = Json.format[LinkedinSearchVcard]

  def create(vcardContent: String): Option[LinkedinSearchVcard] = {
    val image = getImage(vcardContent).getOrElse("")
    val firstName = getFirstName(vcardContent).getOrElse("")
    val lastName = getLastName(vcardContent).getOrElse("")
    val fullName = getFullName(vcardContent).getOrElse("")
    val url = getUrl(vcardContent).getOrElse("")
    val headline = getHeadline(vcardContent).getOrElse("")
    val location = getLocation(vcardContent).getOrElse("")
    val industry = getIndustry(vcardContent).getOrElse("")
    val currentActivities = getCurrentActivities(vcardContent).getOrElse("")
    val pastActivities = getPastActivities(vcardContent).getOrElse("")
    val education = getEducation(vcardContent).getOrElse("")
    val resume = getResume(vcardContent).getOrElse("")
    Some(new LinkedinSearchVcard(image, firstName, lastName, fullName, headline, location, industry, currentActivities, pastActivities, education, resume, url))
  }

  def from(profile: LinkedinProfile): LinkedinSearchVcard = {
    val image = profile.image
    val firstName = profile.firstName
    val lastName = profile.lastName
    val fullName = profile.fullName
    val url = if (profile.canonicalUrl.equals("")) profile.url else profile.canonicalUrl
    val headline = profile.headline
    val location = profile.location
    val industry = profile.industry
    val currentActivities = profile.experiences.filter(e => e.end.equals("")).map(e => e.title + " at " + e.company.name).mkString(", ")
    val pastActivities = profile.experiences.filter(e => !e.end.equals("")).map(e => e.title + " at " + e.company.name).mkString(", ")
    val education = profile.education.map(e => e.school.name).mkString(", ")
    val resume = profile.resumeHTML
    new LinkedinSearchVcard(image, firstName, lastName, fullName, headline, location, industry, currentActivities, pastActivities, education, resume, url)
  }

  def getFirstName(content: String): Option[String] = HTMLMatcher.simple(content, "<h2>\n<strong>\n<a href=\"(?:[^\"]*)\" title=\"(?:[^\"]*)\">\n<span class=\"given-name\">([^<]*)</span> <span class=\"family-name\">(?:[^<]*)</span></a>\n</strong>\n</h2>")(0)
  def getLastName(content: String): Option[String] = HTMLMatcher.simple(content, "<h2>\n<strong>\n<a href=\"(?:[^\"]*)\" title=\"(?:[^\"]*)\">\n<span class=\"given-name\">(?:[^<]*)</span> <span class=\"family-name\">([^<]*)</span></a>\n</strong>\n</h2>")(0)
  def getFullName(content: String): Option[String] = HTMLMatcher.simple(content, "<h2>\n<strong>\n<a href=\"(?:[^\"]*)\" title=\"([^\"]*)\">\n<span class=\"given-name\">(?:[^<]*)</span> <span class=\"family-name\">(?:[^<]*)</span></a>\n</strong>\n</h2>")(0)
  def getUrl(content: String): Option[String] = HTMLMatcher.simple(content, "<h2>\n<strong>\n<a href=\"([^\"]*)\" title=\"(?:[^\"]*)\">\n<span class=\"given-name\">(?:[^<]*)</span> <span class=\"family-name\">(?:[^<]*)</span></a>\n</strong>\n</h2>")(0)
  def getHeadline(content: String): Option[String] = HTMLMatcher.simple(content, "<dd class=\"title\">([^<]*)</dd>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getLocation(content: String): Option[String] = HTMLMatcher.simple(content, "<span class=\"location\">([^<]*)</span>")(0)
  def getIndustry(content: String): Option[String] = HTMLMatcher.simple(content, "<span class=\"industry\">([^<]*)</span>")(0)
  def getCurrentActivities(content: String): Option[String] = HTMLMatcher.simple(content, "<dd class=\"current-content\">\n<span>\n([^<]*)\n</span>\n</dd>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getPastActivities(content: String): Option[String] = HTMLMatcher.simple(content, "<dd class=\"past-content\">\n<span>\n([^<]*)\n</span>\n</dd>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getEducation(content: String): Option[String] = HTMLMatcher.simple(content, "<dd class=\"education-content\">\n<span>\n([^<]*)\n</span>\n</dd>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getResume(content: String): Option[String] = HTMLMatcher.simple(content, "<dd class=\"summary-content\">\n<span>\n([^<]*)\n</span>\n</dd>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getImage(content: String): Option[String] = HTMLMatcher.simple(content, "<a href=\"(?:[^\"]*)\" class=\"profile-photo\" title=\"(?:[^\"]*)\">\n<img src=\"([^\"]*)\"(?: class=\"photo\")?(?: height=\"60\")? width=\"60\"(?: height=\"60\")? alt=\"(?:[^\"]*)\"/>\n</a>")(0)
}
