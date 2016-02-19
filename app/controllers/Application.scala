package controllers

import play.api._
import play.api.mvc._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.HashMap
import barracuda._

class Application extends Controller {

    def index = Action { Ok("hello world") }

    // 無理やりキャッシュする
    var oauthSecret: String = null;

    def twitterAuth = Action.async {
        Twitter.getRequestToken().map { response =>
            val params      = parseQuery(response.body)
            val oauthToken  = params("oauth_token")
            oauthSecret = params("oauth_token_secret")
            Redirect(s"https://api.twitter.com/oauth/authorize?oauth_token=$oauthToken")
        }
    }

    def twitterCallback(oauth_token: String, oauth_verifier: String) = Action.async(
        for {
            _ <- Future { println(oauthSecret) }
            tokenResponse <- Twitter.getAccessToken(oauth_token, oauthSecret, oauth_verifier)
            params <- Future(parseQuery(tokenResponse.body))
            (token, secret) <- Future((params("oauth_token"), params("oauth_token_secret")))
            _ <- Twitter.updateStatus(token, secret, "ライブラリ無しでOAuthの認証は大変だった")
            userTimeline <- Twitter.getUserTimeline(token, secret, "KKKKKeiDrum", 10)
        } yield {
            Ok(userTimeline)
        })

    private def parseQuery(query: String): Map[String, String] =
        query.split("&").map(_.split("=")).map(query => query(0) -> query(1)).toMap

}
