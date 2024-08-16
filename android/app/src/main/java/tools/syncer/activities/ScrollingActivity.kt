package tools.syncer.activities

import android.net.Uri
import android.os.Bundle
import com.google.android.material.appbar.CollapsingToolbarLayout
import com.google.android.material.floatingactionbutton.FloatingActionButton
import com.google.android.material.snackbar.Snackbar
import androidx.appcompat.app.AppCompatActivity
import android.util.Log
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.TextView
import androidx.documentfile.provider.DocumentFile
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import tools.syncer.R
import tools.syncer.databinding.ActivityScrollingBinding

class ScrollingActivity : AppCompatActivity() {

    private lateinit var binding: ActivityScrollingBinding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        val extras = getIntent().getExtras();
        //if (extras != null) {
            //val value = extras.getString("key");
            val value = extras!!.getString("Folder")

            val uri = Uri.parse(value)

            val directory = DocumentFile.fromTreeUri(this, uri!!)
            val files = directory!!.listFiles();
            // System.out.print("")
            //The key argument here must match that used in the other activity
    //}

        try{
            binding = ActivityScrollingBinding.inflate(layoutInflater)
            setContentView(binding.root)

            val rc = this.findViewById<RecyclerView>(R.id.recycler_view);

            //val dataset = arrayOf("January", "February", "March");
            val dataset = files.map { x -> x.uri };

            val customAdapter = CustomAdapter(dataset);

            rc.layoutManager = LinearLayoutManager(this);
            rc.adapter = customAdapter;

            //System.out.print("Hello");

        } catch (e: Exception) {
            Log.i("SAAUTH", e.message!!)
        }
    }
}


class ViewHolder(view: View) : RecyclerView.ViewHolder(view) {
    val imageView: ImageView

    init {
        // Define click listener for the ViewHolder's View
        imageView = view.findViewById(R.id.image_view)
    }
}

class CustomAdapter(private val images: List<Uri>) : RecyclerView.Adapter<ViewHolder>() {
    /**
     * Provide a reference to the type of views that you are using
     * (custom ViewHolder)
     */

    // Create new views (invoked by the layout manager)
    override fun onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): ViewHolder {
        // Create a new view, which defines the UI of the list item
        val view = LayoutInflater.from(viewGroup.context)
                .inflate(R.layout.image_row, viewGroup, false)

        return ViewHolder(view)
    }

    // Replace the contents of a view (invoked by the layout manager)
    override fun onBindViewHolder(viewHolder: ViewHolder, position: Int) {

        // Get element from your dataset at this position and replace the
        // contents of the view with that element
        //viewHolder.textView.text = dataSet[position]
        viewHolder.imageView.setImageURI(images[position])
        //viewHolder.imageView.scaleType = ImageView.ScaleType.CENTER_CROP
    }

    // Return the size of your dataset (invoked by the layout manager)
    override fun getItemCount() = images.size

}
