package scrapers.linkedin.models.profile

import common.HTMLMatcher
import org.apache.commons.lang3.StringEscapeUtils
import play.api.libs.json._

/*
 * TODO : elt has more fields :
 * 	- result
 *  - activities
 */
case class LinkedinProfileEducation(
  id: String,
  school: LinkedinSchool,
  startYear: Int,
  endYear: Int,
  degree: String,
  speciality: String,
  descriptionHTML: String)
object LinkedinProfileEducation {
  implicit val linkedinProfileEducationFormat = Json.format[LinkedinProfileEducation]

  def create(content: String): Option[LinkedinProfileEducation] = {
    val id = getId(content).getOrElse("")
    val school = getSchoolStr(content).flatMap(str => LinkedinSchool.create(str)).getOrElse(new LinkedinSchool("", "", ""))
    val degree = getDegree(content).getOrElse("")
    val speciality = getSpeciality(content).getOrElse("")
    val startYear = getStartYear(content).map(str => str.toInt).getOrElse(-1)
    val endYear = getEndYear(content).map(str => str.toInt).getOrElse(-1)
    val descriptionHTML = getDescriptionHTML(content).getOrElse("")
    Some(new LinkedinProfileEducation(id, school, startYear, endYear, degree, speciality, descriptionHTML))
  }

  def getId(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<div id=\"education-([0-9]*)-view\">")(0)
  def getSchoolStr(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<h4 class=\"summary fn org\" dir=\"auto\">(.*?)</h4>")(0)
  def getDegree(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<span class=\"degree\">([^<]*), </span>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getSpeciality(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<span class=\"major\">" + HTMLMatcher.tagOpt + "([^<]*)" + HTMLMatcher.tagEndOpt + "</span>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getDates(content: String): List[Option[String]] = HTMLMatcher.simple(content, "(?i)<span class=\"education-date\"><time>([0-9]*)</time><time> &#8211; ([0-9]*)</time></span>")
  def getStartYear(content: String): Option[String] = getDates(content)(0)
  def getEndYear(content: String): Option[String] = getDates(content)(1)
  def getDescriptionHTML(content: String): Option[String] = HTMLMatcher.simple(content, "(?is)<p class=\"notes summary-field-show-more\">(.*?)</p>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
}
