package com.example.fragmenttest

import android.os.Bundle
import android.text.Layout
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.fragment.app.Fragment
import com.example.fragmenttest.databinding.LeftFragmentBinding

class LeftFragment: Fragment() {

    public lateinit var binding: LeftFragmentBinding

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        val view = inflater.inflate(R.layout.left_fragment, container, false)
        binding = LeftFragmentBinding.bind(view)
        return view
    }
}