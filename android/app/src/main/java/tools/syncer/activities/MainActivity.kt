package tools.syncer

import SyncerAuthService
import android.app.PendingIntent
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import com.google.android.gms.auth.api.identity.SignInClient
import net.openid.appauth.AppAuthConfiguration
import net.openid.appauth.AuthorizationException
import net.openid.appauth.AuthorizationRequest
import net.openid.appauth.AuthorizationService
import net.openid.appauth.AuthorizationServiceConfiguration
import net.openid.appauth.ResponseTypeValues
import tools.syncer.ui.theme.SyncerTheme

class MainActivity : ComponentActivity(), AuthorizationServiceConfiguration.RetrieveConfigurationCallback {
    private lateinit var oneTapClient: SignInClient
    private lateinit var authService: AuthorizationService;
    private lateinit var config: AuthorizationServiceConfiguration;

    private val EXTRA_FAILED = "failed"
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        setContent {
            SyncerTheme {
                // A surface container using the 'background' color from the theme
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colorScheme.background
                ) {
                    Greeting("Android HELLO")
                }
            }
        }
    }

    override fun onFetchConfigurationCompleted(
        serviceConfiguration: AuthorizationServiceConfiguration?,
        ex: AuthorizationException?
    ) {
        // if(serviceConfiguration != null) {
        //     //println(serviceConfiguration.toJson())
        //     var request = AuthorizationRequest.Builder(
        //         serviceConfiguration,
        //         "964068033680-ks2c69dpna7bcfhe74kdj7umtrg972i9.apps.googleusercontent.com",
        //         ResponseTypeValues.CODE,
        //         Uri.parse("com.googleusercontent.apps.964068033680-ks2c69dpna7bcfhe74kdj7umtrg972i9:/oauth2redirect"),
        //     ).setScopes(listOf("https://www.googleapis.com/auth/youtube.readonly", "openid", "email" ,"profile")).build()

        //     val intentBuilder =
        //         authService.createCustomTabsIntentBuilder(request.toUri()).build()


        //     val completionIntent = Intent(this, SyncHomePageActivity::class.java)
        //     val cancelIntent = Intent(this, MainActivity::class.java)
        //     cancelIntent.putExtra(EXTRA_FAILED, true);
        //     cancelIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        //     val flags = 0

        //     authService.performAuthorizationRequest(
        //         request,
        //         PendingIntent.getActivity(
        //             this,
        //             0,
        //             completionIntent,
        //             flags or PendingIntent.FLAG_MUTABLE
        //         ),
        //         PendingIntent.getActivity(
        //             this,
        //             0,
        //             cancelIntent,
        //             flags or PendingIntent.FLAG_MUTABLE
        //         ),
        //         intentBuilder);

        // }
    }
}

@Composable
fun Greeting(name: String, modifier: Modifier = Modifier) {
    Text(
        text = "Hello $name!",
        modifier = modifier
    )
}

@Preview(showBackground = true)
@Composable
fun GreetingPreview() {
    SyncerTheme {
        Greeting("Android")
    }
}
