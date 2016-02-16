package controllers

import play.api._
import play.api.mvc._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.HashMap
import barracuda._

class Application extends Controller {

  val tokenCache = HashMap[String, String]()

  def index = Action { Ok("hello world") }

  def twitterAuth = Action.async {
    Twitter.getRequestToken().map { response =>
        println(response.body)
        val params      = parseQuery(response.body)
        val oauthToken  = params("oauth_token")
        var oauthSecret = params("oauth_token_secret")
        tokenCache += (oauthToken -> oauthSecret)
        Redirect(s"https://api.twitter.com/oauth/authorize?oauth_token=$oauthToken")
    }
  }

  def twitterCallback(oauth_token: String, oauth_verifier: String) = Action.async(
    for {
        tokenResponse <- Twitter.getAccessToken(oauth_token, oauth_verifier)
        params <- Future(parseQuery(tokenResponse.body))
        userTimeline <- Twitter.getUserTimeline(params("oauth_token"), params("oauth_token_secret"), "KKKKKeiDrum", 10)
    } yield {
        Ok(userTimeline)
    })

  private def parseQuery(query: String): Map[String, String] =
    query.split("&").map(_.split("=")).map(query => query(0) -> query(1)).toMap

}
