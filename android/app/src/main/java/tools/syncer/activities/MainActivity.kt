package tools.syncer

import SyncerAuthService
import android.app.PendingIntent
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.result.ActivityResultLauncher
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

// First time and when we navigate back from scrollingactivity
// First time in oncreate

class MainActivity : ComponentActivity() {
    private lateinit var launcher: ActivityResultLauncher<Uri?>
    //private var executeLauncher : Boolean = true

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        launcher = registerForActivityResult(ActivityResultContracts.OpenDocumentTree()) {uri: Uri? ->

            //executeLauncher = false
            
            val intent = Intent(this, ScrollingActivity::class.java)
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TASK )
            intent.putExtra("Folder", uri.toString());

            startActivity(intent)
        };

        launcher.launch(null);
    }

    override fun onResume() {
        try{
            super.onResume()

            // spagetti
            //if(executeLauncher) {
            
        //}

                //executeLauncher = true
        } catch(e: Exception) {
            Log.i("SAAUTH", e.message!!)
        }
    }
}


