package com.example.uibestpractice

import android.view.LayoutInflater
import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView

class MsgAdapter(val msgList: List<Msg>) : RecyclerView.Adapter<MsgViewHolder>() {

    override fun getItemViewType(position: Int): Int {
        val msg = msgList[position]
        return msg.type
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): MsgViewHolder {
        val layoutInflater = LayoutInflater.from(parent.context)
        if (viewType == Msg.TYPE_RECEIVED) {
            return LeftViewHolder(layoutInflater.inflate(R.layout.msg_left_item, parent, false))
        } else {
            return RightViewHolder(layoutInflater.inflate(R.layout.msg_right_item, parent, false))
        }
    }

    override fun onBindViewHolder(holder: MsgViewHolder, position: Int) {
        val msg = msgList[position]
        when (holder) {
            is LeftViewHolder -> holder.leftMsg.text = msg.content
            is RightViewHolder -> holder.rightMsg.text = msg.content
        }
    }

    override fun getItemCount(): Int = msgList.size
}