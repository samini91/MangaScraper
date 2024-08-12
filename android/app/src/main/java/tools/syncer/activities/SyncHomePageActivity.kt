package tools.syncer

import AuthStateRepository
import SyncerAuthState
import SyncerClient
import android.Manifest.permission.MANAGE_EXTERNAL_STORAGE
import android.Manifest.permission.READ_EXTERNAL_STORAGE
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.os.Environment.isExternalStorageManager
import android.provider.Settings
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.ui.Modifier
import androidx.core.app.ActivityCompat
import com.auth0.android.jwt.JWT
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import net.openid.appauth.AppAuthConfiguration
import net.openid.appauth.AuthState
import net.openid.appauth.AuthorizationException
import net.openid.appauth.AuthorizationResponse
import net.openid.appauth.AuthorizationService
import net.openid.appauth.TokenResponse
import tools.syncer.ui.theme.SyncerTheme

class SyncHomePageActivity : ComponentActivity(), AuthorizationService.TokenResponseCallback {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // if (!isExternalStorageManager()) {
        //     val intent = Intent()
        //     intent.action = Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION
        //     val uri = Uri.fromParts("package", this.packageName, null)
        //     intent.data = uri
        //     startActivity(intent)
        // }

        setContent {
            SyncerTheme {
                // A surface container using the 'background' color from the theme
                Surface(
                        modifier = Modifier.fillMaxSize(),
                        color = MaterialTheme.colorScheme.background
                ) { Greeting("After Login") }
            }
        }
    }

    override fun onStart() {
        super.onStart()

        // val response = AuthorizationResponse.fromIntent(intent)
        // val ex = AuthorizationException.fromIntent(intent)

        // if (ex != null) {
        //     Log.i("SyncHomePageActivity", ex.localizedMessage)
        //     // go back to main activity
        // }

        // if (response != null) {
        //     val request = response.createTokenExchangeRequest()
        //     val appAuth = AppAuthConfiguration.Builder().build()
        //     val authService = AuthorizationService(this, appAuth)
        //     val authState = AuthState(response, ex)
        //     val clientAuthentication = authState.clientAuthentication

        //     authService.performTokenRequest(request, clientAuthentication, this)
        // } else {
        //     // try load auth if not then go back to login
        //     try {
        //         val syncerClient = SyncerClient()
        //         //runBlocking {

        //        GlobalScope.launch {
        //             // figure out to map eithert
        //                 val allFiles = syncerClient.getAllFilesFromDisk()

        //                 allFiles.onRight { files ->
        //                   for (f in files) {
        //                       try {
        //                           syncerClient.downloadMusic(f)
        //                       } catch (e:Exception) {
        //                           Log.e("SYNCER", e.message!!)
        //                           // FILE NAME IS MESSED UP ? is not permitted for example... figure out how to fix this.
        //                       }
        //                   }
        //               }
        //         }
        //     } catch (e: Exception) {
        //         Log.i("SAAUTH", e.message!!)
        //     }
        // }
    }

    override fun onTokenRequestCompleted(response: TokenResponse?, ex: AuthorizationException?) {
        if (response != null && ex == null) {
            if (response.idToken == null || response.refreshToken == null) {
                // relaunch login
                return
            }
            val authStateRepository = AuthStateRepository(this)

            val jwt = JWT(response.idToken!!)

            val syncerAuthState =
                    SyncerAuthState(
                            jwt.subject.toString(),
                            jwt.claims["name"]!!.asString()!!,
                            response.idToken!!,
                            response.refreshToken!!
                    )

            try {
                authStateRepository.saveAuthInfo(syncerAuthState)
            } catch (e: Exception) {
                Log.i("SAAUTH", e.message!!)
            }

        }
    }
}
