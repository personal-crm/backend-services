package dao

import scrapers.linkedin.models.LinkedinProfile
import scala.concurrent._
import ExecutionContext.Implicits.global
import play.api.libs.json._
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api.DB
import reactivemongo.core.commands._

object LinkedinProfileDao {
  private val COLLECTION_NAME = "profiles"
  private def collection()(implicit db: DB): JSONCollection = db.collection[JSONCollection](COLLECTION_NAME)

  //def all()(implicit db: DB): Future[List[LinkedinProfile]] = collection().find(Json.obj()).cursor[JsValue].toList.map(_.map(json => json.asOpt[LinkedinProfile]).flatten)
  //def insert(profile: LinkedinProfile)(implicit db: DB): Future[LastError] = collection().insert(profile)
  def upsert(profile: LinkedinProfile)(implicit db: DB): Future[LastError] = collection().update(Json.obj("id" -> profile.id), profile, upsert = true)
  //def update(profile: LinkedinProfile)(implicit db: DB): Future[LastError] = collection().update(Json.obj("id" -> profile.id), profile)
  //def findById(id: String)(implicit db: DB): Future[Option[LinkedinProfile]] = collection().find(Json.obj("id" -> id)).one[JsValue].map(_.flatMap(_.asOpt[LinkedinProfile]))
  def findByUrl(url: String)(implicit db: DB): Future[Option[LinkedinProfile]] = collection().find(Json.obj("$or" -> Json.arr(Json.obj("url" -> url), Json.obj("canonicalUrl" -> url)))).one[JsValue].map(_.flatMap(_.asOpt[LinkedinProfile]))
  //def remove(id: String)(implicit db: DB): Future[LastError] = collection().remove(Json.obj("id" -> id))
}
