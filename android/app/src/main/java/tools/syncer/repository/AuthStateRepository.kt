import android.content.Context
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.encodeToString

@Serializable
data class SyncerAuthState(val id: String, val name: String, val id_token: String, val refresh_token: String)

class AuthStateRepository(val context: Context) {

    val STORE_NAME = "AuthState"
    val KEY_STATE  = "KeyState"

    fun getAuthInfo(): SyncerAuthState? {
        val prefs = context.getSharedPreferences(STORE_NAME, Context.MODE_PRIVATE)
        val json = prefs.getString(KEY_STATE, null)

        if(json == null) {
            return null
        }

        // satodo move this to general location
        return Json {
            ignoreUnknownKeys = true
        }.decodeFromString<SyncerAuthState>(json)
    }

    fun saveAuthInfo(syncerAuthState: SyncerAuthState) {
        val prefs = context.getSharedPreferences(STORE_NAME, Context.MODE_PRIVATE)
        val editor = prefs.edit()
        val jsonString = Json.encodeToString(syncerAuthState)

        editor.putString(KEY_STATE, jsonString)
        editor.apply()
    }
}
