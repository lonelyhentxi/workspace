package com.example.recyclerview

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.recyclerview.databinding.ActivityHorizontalListBinding
import java.util.*
import kotlin.collections.ArrayList

class HorizontalListActivity : AppCompatActivity() {

    private val fruitNames = listOf(
        "Apple", "Banana", "Orange", "Watermelon", "Pear", "Grape", "Pineapple",
        "Strawberry", "Cherry", "Mango", "Apple", "Banana", "Orange", "Watermelon",
        "Pear", "Grape", "Pineapple", "Strawberry", "Cherry", "Mango"
    )
    private var fruitList = ArrayList<Fruit>()
    private lateinit var binding: ActivityHorizontalListBinding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityHorizontalListBinding.inflate(layoutInflater)
        setContentView(binding.root)
        initFruits()
        val layoutManager = LinearLayoutManager(this)
        layoutManager.orientation = LinearLayoutManager.HORIZONTAL
        binding.lRecyclerView.layoutManager = layoutManager
        val adapter = LFruitAdapter(fruitList)
        binding.lRecyclerView.adapter = adapter
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