package com.example.recyclerview

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import androidx.recyclerview.widget.StaggeredGridLayoutManager
import com.example.recyclerview.databinding.ActivityGridListBinding
import java.lang.StringBuilder
import java.util.*
import kotlin.collections.ArrayList

class GridListActivity : AppCompatActivity() {

    private val fruitNames = listOf(
        "Apple", "Banana", "Orange", "Watermelon", "Pear", "Grape", "Pineapple",
        "Strawberry", "Cherry", "Mango", "Apple", "Banana", "Orange", "Watermelon",
        "Pear", "Grape", "Pineapple", "Strawberry", "Cherry", "Mango"
    )
    private var fruitList = ArrayList<Fruit>()
    private lateinit var binding: ActivityGridListBinding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityGridListBinding.inflate(layoutInflater)
        setContentView(binding.root)
        initFruits()
        val layoutManager = StaggeredGridLayoutManager(3, StaggeredGridLayoutManager.VERTICAL)
        binding.gRecyclerView.layoutManager = layoutManager
        val adapter = GFruitAdapter(fruitList)
        binding.gRecyclerView.adapter = adapter
    }

    private fun initFruits() {
        fruitNames.forEach {
            fruitList.add(
                Fruit(
                    this.getRandomLengthString(it),
                    resources.getIdentifier("${it.toLowerCase(Locale.ENGLISH)}_pic", "drawable", packageName)
                )
            )
        }
    }

    private fun getRandomLengthString (str: String): String {
        val n = (1..20).random()
        val builder = StringBuilder()
        repeat(n) {
            builder.append(str)
        }
        return builder.toString()
    }
}