package tools.syncer.activities

import android.os.Bundle
import com.google.android.material.appbar.CollapsingToolbarLayout
import com.google.android.material.floatingactionbutton.FloatingActionButton
import com.google.android.material.snackbar.Snackbar
import androidx.appcompat.app.AppCompatActivity
import android.util.Log
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import tools.syncer.R
import tools.syncer.databinding.ActivityScrollingBinding

class ScrollingActivity : AppCompatActivity() {

    private lateinit var binding: ActivityScrollingBinding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        try{
            binding = ActivityScrollingBinding.inflate(layoutInflater)
            setContentView(binding.root)

            val rc = this.findViewById<RecyclerView>(R.id.recycler_view);

            val dataset = arrayOf("January", "February", "March");
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
    val textView: TextView

    init {
        // Define click listener for the ViewHolder's View
        textView = view.findViewById(R.id.textView)
    }
}

class CustomAdapter(private val dataSet: Array<String>) : RecyclerView.Adapter<ViewHolder>() {
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
        viewHolder.textView.text = dataSet[position]
    }

    // Return the size of your dataset (invoked by the layout manager)
    override fun getItemCount() = dataSet.size

}
