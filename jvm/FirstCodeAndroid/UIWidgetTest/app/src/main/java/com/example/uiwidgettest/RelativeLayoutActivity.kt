package com.example.uiwidgettest

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import com.example.uiwidgettest.databinding.ActivityRelativeLayoutBinding

class RelativeLayoutActivity : AppCompatActivity() {
    private lateinit var binding: ActivityRelativeLayoutBinding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityRelativeLayoutBinding.inflate(layoutInflater)
        setContentView(binding.root)
        Log.d("RelativeLayoutActivity", "onCreated")
    }
}