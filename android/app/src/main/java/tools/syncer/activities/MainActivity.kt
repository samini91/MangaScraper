package tools.syncer

import SyncerAuthService
import android.app.PendingIntent
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.documentfile.provider.DocumentFile
import com.google.android.gms.auth.api.identity.SignInClient
import net.openid.appauth.AppAuthConfiguration
import net.openid.appauth.AuthorizationException
import net.openid.appauth.AuthorizationRequest
import net.openid.appauth.AuthorizationService
import net.openid.appauth.AuthorizationServiceConfiguration
import net.openid.appauth.ResponseTypeValues
import tools.syncer.ui.theme.SyncerTheme
import tools.syncer.activities.ScrollingActivity
import java.io.File

//private static final int PICKFILE_REQUEST_CODE = 100;

class MainActivity : ComponentActivity() {
    private lateinit var oneTapClient: SignInClient
    private lateinit var authService: AuthorizationService;
    private lateinit var config: AuthorizationServiceConfiguration;

    //private val EXTRA_FAILED = "failed"
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        try{
            val launcher = registerForActivityResult(ActivityResultContracts.OpenDocumentTree()) {uri: Uri? ->
                val directory = DocumentFile.fromTreeUri(this, uri!!)
                val files = directory!!.listFiles();
                System.out.print("Hello");

                val intent = Intent(this, ScrollingActivity::class.java)
                intent.putExtra("Folder", uri.toString());
                startActivity(intent)

            };
            launcher.launch(null);



        } catch (e: Exception) {
            Log.i("ERR", e.message!!)
        }



        // setContent {
        //     SyncerTheme {
        //         // A surface container using the 'background' color from the theme
        //         Surface(
        //             modifier = Modifier.fillMaxSize(),
        //             color = MaterialTheme.colorScheme.background
        //         ) {
        //             Greeting("Android HELLO")
        //         }
        //     }
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
