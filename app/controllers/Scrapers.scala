package controllers

import config.AppCfg
import scrapers.linkedin.ProfileScraper
import scrapers.linkedin.models.LinkedinSearch
import java.util.Date
import scala.concurrent._
import ExecutionContext.Implicits.global
import play.api.libs.json._
import play.api.libs.ws._
import play.api.mvc._
import play.api.Logger
import play.modules.reactivemongo.MongoController

object Scrapers extends Controller with MongoController {
  implicit val DB = db

  def getProfile(url: String) = Action.async {
    val startTime = new Date().getTime()
    ProfileScraper.scrapeOne(url).map { profile =>
      Ok(Json.obj(
        "status" -> 200,
        "execMs" -> (new Date().getTime() - startTime),
        "data" -> profile))
    }
  }

  def relatedProfilesGet(startUrl: String, maxProfiles: Option[Int]) = Action.async {
    relatedProfiles(startUrl, None, maxProfiles)
  }
  def relatedProfilesPost(startUrl: String, maxProfiles: Option[Int]) = Action.async(parse.json) { request =>
    relatedProfiles(startUrl, (request.body \ "ignore").asOpt[List[String]], maxProfiles)
  }
  private def relatedProfiles(startUrl: String, ignoreUrls: Option[List[String]], maxProfiles: Option[Int]) = {
    val max = maxProfiles.getOrElse(1)
    val startTime = new Date().getTime()
    ProfileScraper.scrape(List(), List(startUrl), ignoreUrls.getOrElse(List()), List(), if (max > AppCfg.maxScrapedProfilesAtOnce) AppCfg.maxScrapedProfilesAtOnce else max).map {
      case (scraped, toScrape) =>
        Ok(Json.obj(
          "status" -> 200,
          "execMs" -> (new Date().getTime() - startTime),
          "data" -> Json.obj(
            "nbScraped" -> scraped.size,
            "nbToScrape" -> toScrape.size,
            "scraped" -> scraped,
            "toScrape" -> toScrape)))
    }
  }

  def searchProfile(firstName: String, lastName: String, onlyFrance: Option[Boolean]): Action[AnyContent] = Action.async {
    val startTime = new Date().getTime()
    val a = ProfileScraper.search(firstName, lastName, onlyFrance.isDefined && onlyFrance.get).map { search =>
      Ok(Json.obj(
        "status" -> 200,
        "execMs" -> (new Date().getTime() - startTime),
        "data" -> search))
    }
    a
  }

  def debugPage(url: String) = Action.async {
    Logger.info("FETCH debug: " + url)
    ProfileScraper.fetchLinkedinUrl(url).map { response =>
      val body = response.body
      Ok("GET " + url + "\n\nResponse body:\n" + body)
    }
  }
}
