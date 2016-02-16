package barracuda

import play.Play
import play.api.Play.current
import play.api.libs.ws.WS
import java.net.URLEncoder
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac
import javax.xml.bind.DatatypeConverter
import sun.misc.BASE64Encoder
import scala.concurrent.ExecutionContext.Implicits.global

object Twitter {
    private val endPoint = "https://api.twitter.com"

    val consumerKey    = Play.application().configuration().getString("twitter.consumerKey")
    val consumerSecret = Play.application().configuration().getString("twitter.consumerSecret")

    def getRequestToken() =
        getCredential("POST", "/oauth/request_token", List(
            "oauth_callback"          -> "http://localhost:9000/twitter/callback",
            "oauth_consumer_key"      -> consumerKey,
            "oauth_signature_method"  -> "HMAC-SHA1",
            "oauth_timestamp"         -> (System.currentTimeMillis / 1000).toString,
            "oauth_nonce"             -> System.currentTimeMillis.toString,
            "oauth_version"           -> "1.0"
        ))

    def getAccessToken(oauthToken: String, oauthVerifier: String) =
        getCredential("POST", "/oauth/access_token", List(
            "oauth_consumer_key"      -> consumerKey,
            "oauth_signature_method"  -> "HMAC-SHA1",
            "oauth_timestamp"         -> (System.currentTimeMillis / 1000).toString,
            "oauth_token"             -> oauthToken,
            "oauth_nonce"             -> System.currentTimeMillis.toString,
            "oauth_verifier"           -> oauthVerifier,
            "oauth_version"           -> "1.0"
        ))

    def getUserTimeline(accessToken: String, accessSecret: String, screenName: String, count: Int = 10) =
        getCredential("GET", "/1.1/statuses/user_timeline.json", List(
            "oauth_consumer_key"      -> consumerKey,
            "oauth_signature_method"  -> "HMAC-SHA1",
            "oauth_timestamp"         -> (System.currentTimeMillis / 1000).toString,
            "oauth_token"             -> accessToken,
            "oauth_nonce"             -> System.currentTimeMillis.toString,
            "oauth_version"           -> "1.0"
        ), List(
            "screen_name"             -> screenName,
            "count"                   -> count.toString
        ), accessSecret).map { response =>
            response.body
        }

    private def getCredential(
        requestMethod  : String,
        entryPoint     : String,
        oauthQueries   : List[(String, String)],
        requestQueries : List[(String, String)] = Nil,
        accessSecret   : String = "") =
    {
        val apiUrl = endPoint + entryPoint
        val sortedQueries = (oauthQueries ++ requestQueries).sortWith((a, b) => a._1 < b._1)
        val queryString = joinQueries(sortedQueries, "&")

        val encodedQueryString   = urlEncode(queryString)
        val encodedRequestMethod = urlEncode(requestMethod)
        val encodedApiUrl        = urlEncode(apiUrl)

        val signatureData = s"$encodedRequestMethod&$encodedApiUrl&$encodedQueryString"
        val signatureKey  = s"${urlEncode(consumerSecret)}&${urlEncode(accessSecret)}"
        val signature     = crypto(signatureData, signatureKey)

        val oauthHeader = "OAuth " + joinQueries(("oauth_signature" -> signature) :: sortedQueries, ",")

        val wsObject = WS.url(apiUrl)
            .withQueryString(requestQueries:_*)
            .withHeaders("Authorization" -> oauthHeader)
        requestMethod match {
            case "POST" => wsObject.post("")
            case _      => wsObject.get()
        }
    }

    private def joinQueries(queries: List[(String, String)], separator: String) =
        queries.map(tuple => s"${tuple._1}=${urlEncode(tuple._2)}").mkString(separator)

    private def urlEncode(target: String) = URLEncoder.encode(target, "utf-8")

    private def crypto(target: String, key: String): String = {
        val secret = new SecretKeySpec(key.getBytes(), "HmacSHA1")
        val mac = Mac.getInstance("HmacSHA1")
        mac.init(secret)
        val sha = mac.doFinal(target.getBytes())
        val base64 = new BASE64Encoder()
        base64.encode(sha)
    }


}