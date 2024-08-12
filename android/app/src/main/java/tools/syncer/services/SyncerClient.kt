import android.net.Uri
import android.os.Environment.getExternalStorageDirectory
import android.util.Log
import arrow.core.Either
import arrow.core.right
import kotlinx.serialization.Serializable
import java.io.File

class SyncerClient() {
    val httpClient = BaseHttpClient();
    val basePath = "http://10.0.0.107"
    
    suspend fun saveAuth(syncerAuthState: SyncerAuthState):Either<String, Any> {
        val url = Uri.Builder().scheme(basePath).path("/saveAuth").build()
        return httpClient.post<SyncerAuthState, Any>(url, syncerAuthState)
    }

    suspend fun getAllFilesFromDisk(): Either<String, List<String>> {
        val url = Uri.Builder().scheme(basePath).path("/getFiles").build()
        return httpClient.get<List<String>>(url, HashMap<String, String>())
    }
    
    suspend fun downloadMusic(path: String): Either<String, String> {
        val url = Uri.Builder().scheme(basePath).path("/downloadMusic").appendQueryParameter("path", path).build()

        val pathToDownloadTo = "Music/${path}"

        val file = File("${getExternalStorageDirectory().absolutePath}/${pathToDownloadTo}")

        if(!file.exists())
            return httpClient.downloadFile(url, HashMap<String, String>(), pathToDownloadTo)
        else
            return Either.Right("File Already Exists!")
    }

    // client.prepareGet("https://ktor.io/").execute { httpResponse ->
    //     val channel: ByteReadChannel = httpResponse.body()
    //     while (!channel.isClosedForRead) {
    //         val packet = channel.readRemaining(DEFAULT_BUFFER_SIZE.toLong())
    //         while (!packet.isEmpty) {
    //             val bytes = packet.readBytes()
    //             file.appendBytes(bytes)
    //             println("Received ${file.length()} bytes from ${httpResponse.contentLength()}")
    //         }
    //     }
    //     println("A file saved to ${file.path}")
    // }

    // suspend fun downloadMusic(): Either<String, List<String>> {
        
    // }


}
