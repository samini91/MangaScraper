import android.content.Context
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.encodeToString

import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.request
import io.ktor.client.statement.HttpResponse

class SyncerAuthService(val context:Context) {
    val authStateRepository = AuthStateRepository(context)
    val syncerClient = SyncerClient()

    fun getAuth(): SyncerAuthState? {
        return authStateRepository.getAuthInfo();
    }

    suspend fun saveAuth(syncerAuthState: SyncerAuthState) {
        authStateRepository.saveAuthInfo(syncerAuthState)
        syncerClient.saveAuth(syncerAuthState)
    }
}


