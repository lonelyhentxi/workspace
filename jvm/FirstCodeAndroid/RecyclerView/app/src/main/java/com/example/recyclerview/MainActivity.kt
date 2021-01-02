package com.example.recyclerview

import android.content.Intent
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.recyclerview.databinding.ActivityMainBinding
import java.util.*
import kotlin.collections.ArrayList

class MainActivity : AppCompatActivity() {

    private val fruitNames = listOf(
        "Apple", "Banana", "Orange", "Watermelon", "Pear", "Grape", "Pineapple",
        "Strawberry", "Cherry", "Mango", "Apple", "Banana", "Orange", "Watermelon",
        "Pear", "Grape", "Pineapple", "Strawberry", "Cherry", "Mango"
    )
    private var fruitList = ArrayList<Fruit>()
    private lateinit var binding: ActivityMainBinding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)
        initFruits()
        // use linearLayoutManager when actually use constraintLayout as well
        val layoutManager = LinearLayoutManager(this)
        binding.recyclerView.layoutManager = layoutManager
        val adapter = FruitAdapter(fruitList)
        binding.recyclerView.adapter = adapter

        binding.forwardHorizontalListButton.setOnClickListener {
            val intent = Intent(this, HorizontalListActivity::class.java)
            startActivity(intent)
        }

        binding.forwardGridListButton.setOnClickListener {
            val intent = Intent(this, GridListActivity::class.java)
            startActivity(intent)
        }
    }

    private fun initFruits() {
        fruitNames.forEach {
            fruitList.add(
                Fruit(
                    it,
                    resources.getIdentifier("${it.toLowerCase(Locale.ENGLISH)}_pic", "drawable", packageName)
                )
            )
        }
    }
}