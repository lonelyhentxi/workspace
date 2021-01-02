package com.example.uiwidgettest

import android.content.Intent
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import android.view.View
import android.widget.Toast
import androidx.appcompat.app.AlertDialog
import com.example.uiwidgettest.databinding.ActivityMainBinding

class MainActivity : AppCompatActivity(), View.OnClickListener {
    private lateinit var binding: ActivityMainBinding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)
        binding.button.setOnClickListener(this)
        binding.changeProgress.setOnClickListener(this)
        binding.goToRelativeLayout.setOnClickListener(this)
        binding.goToRelativeLayout2.setOnClickListener(this)
        binding.goToFrameLayout.setOnClickListener(this)
        Log.d("MainActivity", "onCreated")
    }

    override fun onClick(v: View?) {
        when (v?.id) {
            R.id.button -> {
                val inputText = binding.editText.text.toString()
                Toast.makeText(this, inputText, Toast.LENGTH_SHORT).show();
                binding.imageView.setImageResource(R.drawable.img_2)
            }
            R.id.changeProgress -> {
                Log.d(
                    "MainActivity",
                    "changeProgress progress ${binding.progressBar.progress}"
                )
                val currentProgress = binding.progressBar.progress;
                if (currentProgress >= 100) {
                    AlertDialog.Builder(this).apply {
                        setTitle("Progress full")
                        setMessage("hide progress bar?")
                        setCancelable(false)
                        setPositiveButton("Ok") { dialog, which -> binding.progressBar.visibility = View.INVISIBLE }
                        setNegativeButton("Cancel") { dialog, which -> binding.progressBar.visibility = View.VISIBLE }
                        show()
                    }
                } else {
                    binding.progressBar.progress = binding.progressBar.progress + 10
                }
            }
            R.id.goToRelativeLayout -> {
                val intent = Intent(this, RelativeLayoutActivity::class.java)
                startActivity(intent)
            }
            R.id.goToRelativeLayout2 -> {
                val intent = Intent(this, RelativeLayoutActivity2::class.java)
                startActivity(intent)
            }
            R.id.goToFrameLayout -> {
                val intent = Intent(this, FrameLayoutActivity::class.java)
                startActivity(intent)
            }
        }
    }
}