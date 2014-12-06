package scrapers.linkedin.models

import scrapers.linkedin.models.search.LinkedinSearchVcard
import common.HTMLMatcher
import play.api.libs.json._

case class LinkedinSearch(
  firstName: String,
  lastName: String,
  url: String,
  nbResults: Int,
  totalResults: Int,
  results: List[LinkedinSearchVcard])
object LinkedinSearch {
  implicit val linkedinSearchFormat = Json.format[LinkedinSearch]

  def getUrl(firstName: String, lastName: String) = "https://www.linkedin.com/pub/dir/?first=" + firstName + "&last=" + lastName
  def getUrlForFrance(firstName: String, lastName: String) = "https://fr.linkedin.com/pub/dir/" + firstName + "/" + lastName + "/fr-0-France"

  def create(firstName: String, lastName: String, url: String, content: String): Option[LinkedinSearch] = {
    val profile = LinkedinProfile.create(url, content)
    if (profile.isDefined) {
      Some(new LinkedinSearch(firstName, lastName, url, 1, 1, List(LinkedinSearchVcard.from(profile.get))))
    } else {
      val vcards = getVcardsStr(content).map(vcard => LinkedinSearchVcard.create(vcard)).flatten
      val totalResults = getTotalResults(content).map(str => str.toInt).getOrElse(0)
      Some(new LinkedinSearch(firstName, lastName, url, vcards.size, totalResults, vcards))
    }
  }

  def getVcardsStr(content: String): List[String] = HTMLMatcher.multi(content, "(?is)<li class=\"vcard\">(.*?)</li>").map(vcard => vcard(0)).flatten
  def getTotalResults(content: String): Option[String] = HTMLMatcher.simple(content, "<div class=\"pagination\"> (?:[0-9]+) profil\\(s\\) sur ([0-9]+) affiché\\(s\\)")(0)
}
