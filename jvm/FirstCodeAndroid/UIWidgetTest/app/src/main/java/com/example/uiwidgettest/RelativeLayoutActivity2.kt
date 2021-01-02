package com.example.uiwidgettest

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import com.example.uiwidgettest.databinding.ActivityMainBinding
import com.example.uiwidgettest.databinding.ActivityRelativeLayout2Binding

class RelativeLayoutActivity2 : AppCompatActivity() {
    private lateinit var binding: ActivityRelativeLayout2Binding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityRelativeLayout2Binding.inflate(layoutInflater)
        setContentView(binding.root)
        Log.d("RelativeLayoutActivity2", "onCreated")
    }
}