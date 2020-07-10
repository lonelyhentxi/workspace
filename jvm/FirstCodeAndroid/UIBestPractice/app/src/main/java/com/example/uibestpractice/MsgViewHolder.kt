package com.example.uibestpractice

import android.view.View
import androidx.recyclerview.widget.RecyclerView
import com.example.uibestpractice.databinding.MsgLeftItemBinding
import com.example.uibestpractice.databinding.MsgRightItemBinding

sealed class MsgViewHolder (view: View): RecyclerView.ViewHolder(view)

class LeftViewHolder (view: View): MsgViewHolder(view) {
    val leftMsg = MsgLeftItemBinding.bind(view).leftMsg
}

class RightViewHolder (view: View): MsgViewHolder(view) {
    val rightMsg = MsgRightItemBinding.bind(view).rightMsg
}