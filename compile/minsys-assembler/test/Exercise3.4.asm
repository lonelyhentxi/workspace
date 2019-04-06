.DATA 0x0
	ZIZENGDEY:	.word 0x007F2812		#;据CPU时钟周期60ns，1s需延时计数值为16666667，实际需要8333330个(007F2812H)。
	ZIJIANDEY	:	.word 0x007F2811
	ZUOYIDEY:	.word 0x007F2810
	LUOYOUDEY: 	.word 0x007F2810
	SUANYOUDEY:	.word 0x007F280F

	FUZHI: 		.word 0x00000001	#;赋值
	ZIZENG: 		.word 0x00000002	#;自增
	ZIJIAN:		.word 0x00000003	#;自减
	ZUOYI: 		.word 0x00000005	#;左移
	LUOYOU: 		.word 0x00000006	#;逻辑右移
	SUANYOU: 	.word 0x00000007	#;算术右移
	LOWONE: 	.word 0x0000FFFF

.TEXT 0x0000
START:
	lui   	$1,0XFFFF				#;让$28为0FFFF0000H作为端口地址的高16位
        ori   	$28,$1,0XF000			#;$28端口是系统的I/O地址的高20位
	lw 	$10,FUZHI($0)
	lw 	$11,ZIZENG($0)
	lw 	$12,ZIJIAN($0)
	lw 	$13,ZUOYI($0)
	lw 	$14,LUOYOU($0)
	lw 	$15,SUANYOU($0)
	lw 	$21,LOWONE($0)
REFRESH:
	lw 	$1,0X0C70($28) 			#;取sw开关值到1号寄存器
	srl 	$2,$1,13  				#;取出新的控制位
	beq 	$2,$10,FUZHIBLOCK 		#;跳转赋值块
	beq 	$2,$11,ZIZENGBLOCK 		#;跳转自增块
	beq 	$2,$12,ZIJIANBLOCK 		#;跳转自减块
	beq 	$2,$13,ZUOYIBLOCK 		#;跳转左移块
	beq 	$2,$14,LUOYOUBLOCK 	#;跳转逻辑右移块
	beq 	$2,$15,SUANYOUBLOCK 	#;跳转算数右移块
	j WAIT 					#;若上述都未进入，则为无效控制位，取下一个SW开关的值
ZIZENGBLOCK:
	addi 	$4,$4,1 				#;VAL加1
	lw     $29,ZIZENGDEY($0)
lop0:	addi	$29,$29,-1
	bne	$29,$0,lop0
	j WAIT
ZIJIANBLOCK:
	addiu	$4,$4,-1 				#;VAL减1
	lw     $29,ZIJIANDEY($0)
lop1:	addi	$29,$29,-1
	bne	$29,$0,lop1
	j WAIT
ZUOYIBLOCK:
	sll 	$4,$4,1 					#;VAL左移一位
	and 	$4,$4,$21 				#;高16位取0，防止再逻辑右移时高位出现1
	lw     $29,ZUOYIDEY($0)
lop2:	addi	$29,$29,-1
	bne	$29,$0,lop2
	j WAIT
FUZHIBLOCK:
	lw   	$1,0XC70($28)			#;从拨码开关读取数据
	andi	$4,$1,0X1FFF				#;获取拨码开关0-12位
	j WAIT
LUOYOUBLOCK:
	srl 	$4,$4,1 					#;VAL逻辑右移
	lw     $29,LUOYOUDEY($0)
lop3:	addi	$29,$29,-1
	bne	$29,$0,lop3
	j WAIT
SUANYOUBLOCK:
	sll 	$5,$4,16 					#;先左移16位到5号寄存器的高位，以便sra获取低16位的符号位
	sra 	$5,$5,1 					#;再算术右移1位
	srl 	$4,$5,16 					#;再逻辑右移16位回写4号寄存器用以显示
	lw     $29,SUANYOUDEY($0)
lop4:	addi	$29,$29,-1
	bne	$29,$0,lop4
	j WAIT
WAIT:
	sw 	$4,0XC60($28) 			#;将4号寄存器中VAL的值送入LED灯
	j REFRESH

