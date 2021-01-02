package com.example.listviewtest

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.Toast
import com.example.listviewtest.databinding.ActivityMainBinding
import java.util.*
import kotlin.collections.ArrayList

class MainActivity : AppCompatActivity() {
    private lateinit var binding: ActivityMainBinding
    private val fruitNames = listOf(
        "Apple", "Banana", "Orange", "Watermelon", "Pear", "Grape", "Pineapple",
        "Strawberry", "Cherry", "Mango", "Apple", "Banana", "Orange", "Watermelon",
        "Pear", "Grape", "Pineapple", "Strawberry", "Cherry", "Mango"
    )
    private var fruits = ArrayList<Fruit>()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)
        initFruits()
        val adapter = FruitAdapter(this, R.layout.fruit_item, fruits)
        binding.listView.adapter = adapter
        binding.listView.setOnItemClickListener { _, _, position, _ ->
            val fruit = fruits[position]
            Toast.makeText(this, fruit.name, Toast.LENGTH_SHORT).show()
        }
    }

    private fun initFruits() {
        fruitNames.forEach {
            fruits.add(
                Fruit(
                    it,
                    resources.getIdentifier("${it.toLowerCase(Locale.ENGLISH)}_pic", "drawable", packageName)
                )
            )
        }
    }
}