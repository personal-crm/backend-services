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
import com.ning.http.client.MaxRedirectException

object ProfileScraper {
  def scrape(results: List[LinkedinProfile], urlsToScrape: List[String], urlsToIgnore: List[String], alreadyScraped: List[String], max: Int)(implicit db: DB): Future[(List[LinkedinProfile], List[String])] = {
    if (results.size >= max || urlsToScrape.size == 0) {
      Future.successful((results, urlsToScrape))
    } else {
      val toScrape = urlsToScrape.take(max - results.size)
      val scrapedFuture = Future.sequence(toScrape.map { url =>
        if (alreadyScraped.contains(url)) Future.successful(None)
        else scrapeOne(url)
      })
      scrapedFuture.flatMap { scrapedOpts =>
        val scraped = scrapedOpts.flatten
        val newUrlsToScrape = scraped.flatMap { profile => profile.relatedProfiles.map(related => related.link) }
        scrape(
          results ++ scraped.filter(p => !urlsToIgnore.contains(p.url)),
          (urlsToScrape.drop(max - results.size) ++ newUrlsToScrape).distinct,
          urlsToIgnore,
          alreadyScraped ++ scraped.map(p => p.url),
          max)
      }
    }
  }

  def scrapeOne(url: String)(implicit db: DB): Future[Option[LinkedinProfile]] = {
    val cachedProfile = if (AppCfg.useMongoCache) LinkedinProfileDao.findByUrl(url) else Future.successful(None)
    cachedProfile.flatMap { profileOpt =>
      if (profileOpt.isDefined) {
        Future.successful(profileOpt)
      } else {
        Logger.info("FETCH profile: " + url)
        fetchLinkedinUrl(url).map { response =>
          val profileOpt = LinkedinProfile.create(url, response.body)
          if (profileOpt.isDefined) {
            LinkedinProfileDao.upsert(profileOpt.get)
          }
          profileOpt
        }.recover {
          case e: Exception => Logger.error("catched " + e.getClass().getName() + ": " + e.getMessage()); None
        }
      }
    }
  }

  def search(firstName: String, lastName: String, onlyFrance: Boolean): Future[Option[LinkedinSearch]] = {
    val url = if (onlyFrance) LinkedinSearch.getUrlForFrance(firstName, lastName) else LinkedinSearch.getUrl(firstName, lastName)
    Logger.info("FETCH search: " + url)
    fetchLinkedinUrl(url).map { response =>
      LinkedinSearch.create(firstName, lastName, url, response.body)
    }.recover {
      case e: Exception => Logger.error("catched " + e.getClass().getName() + ": " + e.getMessage()); None
    }
  }

  def fetchLinkedinUrl(url: String): Future[Response] = {
    WS.url(url).withHeaders(
      //"Accept" -> "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
      //"Accept-Encoding" -> "gzip,deflate,sdch",
      //"Connection" -> "keep-alive",
      //"Host" -> "www.linkedin.com",
      //"User-Agent" -> "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/38.0.2125.104 Safari/537.36",
      "Accept-Language" -> "fr-FR,fr;q=0.8,en-US;q=0.6,en;q=0.4").get()
  }
}
