.data
   a:  .word  3
.text  0x0000						#;代码开始的首地址
start: lui   $1,0xffff					#;让$28为0FFFF0000H作为端口地址的高16位
        ori   $28,$1,0xF000				        #;$28端口是系统的I/O地址的高20位
switled:								#;测试led和拨码开关
	lw   $1,0xC70($28)					#;从拨码开关读取数据
	sw   $1,0xC60($28)					#;将拨码开端的数据写到led上
	j switled
