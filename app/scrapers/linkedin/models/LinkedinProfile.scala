package scrapers.linkedin.models

import common.Utils
import common.HTMLMatcher
import scrapers.linkedin.models.profile.LinkedinProfileSummary
import scrapers.linkedin.models.profile.LinkedinProfileExperience
import scrapers.linkedin.models.profile.LinkedinProfileEducation
import scrapers.linkedin.models.profile.LinkedinCompany
import scrapers.linkedin.models.profile.LinkedinSchool
import scrapers.linkedin.models.profile.LinkedinWebsite
import scrapers.linkedin.models.profile.LinkedinRelatedProfile
import java.util.Date
import java.net.URLDecoder
import org.apache.commons.lang3.StringEscapeUtils
import play.api.Logger
import play.api.libs.json._

/*
 * TODO : get more data from the profile :
 * 	- volunteering
 *  - organizations
 *  - honors
 *  - certifications
 *  - skills
 *  - languages
 *  - patents
 *  - publications
 *  - test-scores
 *  - courses
 */
case class LinkedinProfile(
  id: String,
  image: String,
  imageFull: String,
  firstName: String,
  lastName: String,
  fullName: String,
  headline: String,
  location: String,
  industry: String,
  nbConnections: Int,
  summary: LinkedinProfileSummary,
  resumeHTML: String,
  experiences: List[LinkedinProfileExperience],
  education: List[LinkedinProfileEducation],
  relatedProfiles: List[LinkedinRelatedProfile],
  url: String,
  canonicalUrl: String,
  updated: Long) {
  override def toString = "LinkedinProfile(" + this.id + ")"
}

object LinkedinProfile {
  implicit val linkedinProfileFormat = Json.format[LinkedinProfile]

  def create(url: String, content: String): Option[LinkedinProfile] = {
    val id = getId(content).getOrElse("")
    if (id.length > 0) {
      val image = getImage(content).getOrElse("https://static.licdn.com/scds/common/u/images/themes/katy/ghosts/person/ghost_person_100x100_v1.png")
      val imageFull = getImageFull(content).getOrElse("")
      val firstName = getFirstName(content).getOrElse("")
      val lastName = getLastName(content).getOrElse("")
      val fullName = getFullName(content).getOrElse("")
      val headline = getHeadline(content).getOrElse("")
      val location = getLocation(content).getOrElse("")
      val industry = getIndustry(content).getOrElse("")
      val nbConnections = getNbConnections(content).map(str => str.toInt).getOrElse(-1)
      val summary = getSummaryStr(content).flatMap(str => LinkedinProfileSummary.create(str)).getOrElse(new LinkedinProfileSummary(List(), List(), List(), List()))
      val resumeHTML = getResumeHTML(content).getOrElse("")
      val experiences = getExperiencesStr(content).map(elt => LinkedinProfileExperience.create(elt)).flatten
      val education = getEducationsStr(content).map(elt => LinkedinProfileEducation.create(elt)).flatten
      val relatedProfiles = getRelatedProfilesStr(content).map(elt => LinkedinRelatedProfile.create(elt)).flatten
      val canonicalUrl = getCanonicalUrl(content).getOrElse(url)
      Some(new LinkedinProfile(id, image, imageFull, firstName, lastName, fullName, headline, location, industry, nbConnections, summary, resumeHTML, experiences, education, relatedProfiles, url, canonicalUrl, new Date().getTime()))
    } else {
      None
    }
  }

  def getId(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<div id=\"member-([0-9]+)\" class=\"masthead\">")(0)
  def getFirstName(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<a href=\"(?:http://www.linkedin.com)?/pub/dir/\\?first=([^&]*)&amp;last=(?:[^\"]*)\">Voir plus</a>")(0).map(str => URLDecoder.decode(str, "UTF-8"))
  def getLastName(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<a href=\"(?:http://www.linkedin.com)?/pub/dir/\\?first=(?:[^&]*)&amp;last=([^\"]*)\">Voir plus</a>")(0).map(str => URLDecoder.decode(str, "UTF-8"))
  def getFullName(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<span class=\"full-name\" dir=\"auto\">([^<]*)</span>")(0)
  def getImage(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<img src='([^']*)'(?: width=\"200\" height=\"200\")? alt=\"[^\"]*\"/>")(0)
  def getImageFull(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<img src=\"([^\"]*)\" width=\"500\" height=\"500\" id=\"bg-blur-profile-picture\" alt=\"[^\"]*\"/>")(0)
  def getHeadline(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<p class=\"title\">([^<]*)</p>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getLocation(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<span class=\"locality\">([^<]*)</span>")(0)
  def getIndustry(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<dd class=\"industry\">([^<]*)</dd>")(0)
  def getNbConnections(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<div class=\"member-connections\"><strong>(?:\\+ de )?([0-9]+)</strong>relations</div>")(0)
  def getSummaryStr(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<table summary=\"(?:[^\"]*)\">(.*?)</table>")(0)
  def getResumeHTML(content: String): Option[String] = HTMLMatcher.simple(content, "(?si)<div class=\"summary\"><p class=\"description\">(.*?)</p>")(0).map(str => StringEscapeUtils.unescapeHtml4(str))
  def getExperiencesStr(content: String): List[String] = HTMLMatcher.multi(content, "(?si)(<div id=\"experience-[0-9]*-view\">.*?</div>)").map(matches => matches(0)).flatten
  def getEducationsStr(content: String): List[String] = HTMLMatcher.multi(content, "(?si)(<div id=\"education-[0-9]*-view\">.*?</div>)").map(matches => matches(0)).flatten
  def getRelatedProfilesStr(content: String): List[String] =
    HTMLMatcher.simple(content, "(?i)<div class=\"insights-browse-map\"><h3>Autres pages consultées</h3><ul>(.*?)</ul></div>")(0)
      .map(str => HTMLMatcher.multi(str, "(?i)<li(?: class=\"first\")?>(.*?)</li>").map(matches => matches(0)).flatten).getOrElse(List())
  def getCanonicalUrl(content: String): Option[String] = HTMLMatcher.simple(content, "(?i)<link rel=\"canonical\" href=\"([^\"]+)\"/>")(0)
}
