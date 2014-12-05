package scrapers.linkedin

import config.AppCfg
import common.Utils
import scrapers.linkedin.models.LinkedinProfile
import scrapers.linkedin.models.LinkedinSearch
import dao.LinkedinProfileDao
import scala.concurrent._
import ExecutionContext.Implicits.global
import play.api.libs.ws._
import play.api.Logger
import reactivemongo.api.DB

object ProfileScraper {
  def scrape(results: Map[String, LinkedinProfile], urlsToScrape: List[String], max: Int)(implicit db: DB): Future[(Map[String, LinkedinProfile], List[String])] = {
    if (results.size >= max || urlsToScrape.size == 0) {
      Future.successful((results, urlsToScrape))
    } else {
      val toScrape = urlsToScrape.take(max - results.size)
      val scrapedFuture = Future.sequence(toScrape.map { url =>
        if (results.get(url).isDefined) Future.successful(None)
        else scrapeOne(url)
      }.map(_.map(_.map(e => (e.url, e)))))
      scrapedFuture.flatMap { scrapedOpts =>
        val scraped = scrapedOpts.flatten
        val newUrlsToScrape = scraped.flatMap { case (url, profile) => profile.relatedProfiles.map(related => related.link) }
        scrape(results ++ scraped, (urlsToScrape.drop(max - results.size) ++ newUrlsToScrape).distinct, max)
      }
    }
  }

  def scrapeOne(url: String)(implicit db: DB): Future[Option[LinkedinProfile]] = {
    val cachedProfile = if (AppCfg.useMongoCache) LinkedinProfileDao.findByUrl(url) else Future.successful(None)
    cachedProfile.flatMap { profileOpt =>
      if (profileOpt.isDefined) {
        Future.successful(profileOpt)
      } else {
        Logger.info("FETCH: " + url)
        fetchLinkedinUrl(url).map { response =>
          val profileOpt = LinkedinProfile.create(url, response.body)
          if (profileOpt.isDefined) {
            LinkedinProfileDao.upsert(profileOpt.get)
          }
          profileOpt
        }
      }
    }
  }

  def search(firstName: String, lastName: String, onlyFrance: Boolean): Future[Option[LinkedinSearch]] = {
    val url = if (onlyFrance) LinkedinSearch.getUrlForFrance(firstName, lastName) else LinkedinSearch.getUrl(firstName, lastName)
    Logger.info("FETCH: " + url)
    fetchLinkedinUrl(url).map { response =>
      LinkedinSearch.create(firstName, lastName, url, response.body)
    }
  }

  def fetchLinkedinUrl(url: String): Future[Response] = {
    WS.url(url).withHeaders(
      //"Accept" -> "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
      //"Accept-Encoding" -> "gzip,deflate,sdch",
      //"Connection" -> "keep-alive",
      //"Host" -> "www.linkedin.com",
      //"User-Agent" -> "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/38.0.2125.104 Safari/537.36"
      "Accept-Language" -> "fr-FR,fr;q=0.8,en-US;q=0.6,en;q=0.4").get()
  }
}
