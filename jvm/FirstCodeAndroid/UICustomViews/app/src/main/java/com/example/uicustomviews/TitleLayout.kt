package com.example.uicustomviews

import android.app.Activity
import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.widget.Toast
import androidx.constraintlayout.widget.ConstraintLayout
import com.example.uicustomviews.databinding.TitleBinding

class TitleLayout(context: Context, attrs: AttributeSet): ConstraintLayout (context, attrs) {
    private lateinit var binding: TitleBinding
    init {
        val layoutInflater = LayoutInflater.from(context)
        val layoutView = layoutInflater.inflate(R.layout.title, this)
        binding = TitleBinding.bind(layoutView)
        binding.titleBack.setOnClickListener {
            val activity = context as Activity
            activity.finish()
        }
        binding.titleEdit.setOnClickListener {
            Toast.makeText(context, "You clicked Edit Button", Toast.LENGTH_SHORT).show()
        }
    }
}