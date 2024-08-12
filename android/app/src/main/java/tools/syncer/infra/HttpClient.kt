import android.net.Uri
import android.os.Environment.getExternalStorageDirectory
import android.util.Log
import arrow.core.Either
import io.ktor.client.*
import io.ktor.client.call.body
import io.ktor.client.engine.cio.*
import io.ktor.client.plugins.HttpTimeout
import io.ktor.client.plugins.contentnegotiation.ContentNegotiation
import io.ktor.client.plugins.timeout
import io.ktor.client.request.get
import io.ktor.client.request.post
import io.ktor.client.request.setBody
import io.ktor.client.request.prepareGet
import io.ktor.client.statement.HttpResponse
import io.ktor.client.statement.bodyAsText
import io.ktor.http.ContentType
import io.ktor.http.contentType
import io.ktor.serialization.kotlinx.json.json
import io.ktor.utils.io.ByteReadChannel
import io.ktor.utils.io.core.isEmpty
import io.ktor.utils.io.core.readBytes
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import java.io.File

class BaseHttpClient() {
    val client = HttpClient(CIO) {
        install(ContentNegotiation) { json() }
        install(HttpTimeout)
    }

    suspend inline fun <reified Req, reified Res> post(
            url: Uri,
            body: Req
    ): Either<String, Res> {
        val response: HttpResponse =
                client.post(url.toString()) {
                    contentType(ContentType.Application.Json)
                    setBody(body)
                }

        // SATODO map all error codes to types
        if (response.status.value in 200..299) {
            val res: Res = response.body()
            return Either.Right(res)
        }

        return Either.Left(response.bodyAsText())
    }

    // SATODO dictionary is prob not he best thing here
    suspend inline fun <reified Res> get(
            url: Uri,
            queryParams: HashMap<String, String>
    ): Either<String, Res>  {

        val response: HttpResponse =
                client.get(url.toString()) {
                    contentType(ContentType.Application.Json)
                    url { queryParams.forEach { (k, v) -> parameters.append(k, v) } }
                }

        // SATODO map all error codes to types
        if (response.status.value in 200..299) {
            val res: Res = response.body()
            return Either.Right(res)
        }

        return Either.Left(response.bodyAsText())
                                     }

    suspend fun downloadFile(
        url: Uri,
        queryParams: HashMap<String, String>,
        pathToDownloadTo: String,
    ):Either<String, String> {

        val file = File("${getExternalStorageDirectory().absolutePath}/${pathToDownloadTo}")

        file.parentFile.mkdirs()

        file.createNewFile()

        client.prepareGet(url.toString()) {
            url { queryParams.forEach { (k, v) -> parameters.append(k, v) } }
            timeout { requestTimeoutMillis = HttpTimeout.INFINITE_TIMEOUT_MS }
        }.execute { httpResponse ->
            Log.i("SYNCER", "Starting download of: ${file.path}")
            val channel: ByteReadChannel = httpResponse.body()
            while (!channel.isClosedForRead) {
                val packet = channel.readRemaining(DEFAULT_BUFFER_SIZE.toLong())
                while (!packet.isEmpty) {
                    val bytes = packet.readBytes()
                    file.appendBytes(bytes)
                    //println("Received ${file.length()} bytes from ${httpResponse.contentLength()}")
                }
            }
            println("A file saved to ${file.path}")
            Log.i("SYNCER", "A file saved to ${file.path}")
        }

        return Either.Right("File Downloaded")
    }
}
