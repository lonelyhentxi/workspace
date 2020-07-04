package com.example.activitylifecycletest

import android.content.Intent
import android.os.Bundle
import android.os.PersistableBundle
import android.util.Log
import androidx.appcompat.app.AppCompatActivity
import kotlinx.android.synthetic.main.main_layout.*

class MainActivity : AppCompatActivity() {

    private val tag = "MainActivity"

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        Log.d(this.tag, "onCreate")
        setContentView(R.layout.main_layout)
        if (savedInstanceState != null) {
            val tempData = savedInstanceState.getString("data_key")
            Log.d(this.tag, "tempData is $tempData")
        }
        startNormalActivity.setOnClickListener {
            val intent = Intent(this, NormalActivity::class.java)
            startActivity(intent)
        }
        startDialogActivity.setOnClickListener {
            val intent = Intent(this, DialogActivity::class.java)
            startActivity(intent)
        }
    }

    override fun onStart() {
        super.onStart()
        Log.d(this.tag, "onStart")
    }

    override fun onResume() {
        super.onResume()
        Log.d(this.tag, "onResume")
    }

    override fun onPause() {
        super.onPause()
        Log.d(this.tag, "onPause")
    }

    override fun onStop() {
        super.onStop()
        Log.d(this.tag, "onStop")
    }

    override fun onDestroy() {
        super.onDestroy()
        Log.d(this.tag, "onDestroy")
    }

    override fun onRestart() {
        super.onRestart()
        Log.d(this.tag, "onRestart")
    }

    override fun onSaveInstanceState(outState: Bundle, outPersistentState: PersistableBundle) {
        super.onSaveInstanceState(outState, outPersistentState)
        val tempData = "Something you just typed"
        outState.putString("data_key", tempData)
    }
}