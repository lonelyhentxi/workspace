.DATA  0x0000               					#;数据段定义开始
	DELAY:	.word 0x003F9409            		#;延时计数的时钟周期,针对本CPU时钟周期是60ns，需要消耗8333333个时钟周期才到0.5秒
	LED0: 	.word 0x00000000			#	;通过指令的执行实际的延时计数值只需要4166665个(003F9409H)即可，因为指令执行本
	LED2:	.word 0x00008001			#		;身就消耗时钟周期
	LED4:	.word 0x0000C003
	LED6:	.word 0x0000E007
	LED8:	.word 0x0000F00F
	LED10:	.word 0x0000F81F
	LED12:	.word 0x0000FC3F
	LED14:	.word 0x0000FE7F
	LED16:	.word 0x0000FFFF
.TEXT  0X0000				      			#	;代码段定义开始
start:lui   	$1,0XFFFF				#		;让$28为0FFFF0000H作为端口地址的高16位
	ori   	$28,$1,0XF000				#       ;$28端口是系统的I/O地址的高20位
	lw    	$25,LED0($0)
	lw    	$24,LED2($0)
	lw    	$23,LED4($0)
	lw    	$22,LED6($0)
	lw    	$21,LED8($0)
	lw    	$20,LED10($0)
	lw    	$19,LED12($0)
	lw    	$18,LED14($0)
	lw    	$17,LED16($0)

dely:	lw    	$29,DELAY($0)
loop:	addi	$29,$29,-1
	bne	$29,$0,loop
	jr 	$31
writ:	sw	$24,0X0C60($28)
	jal	dely
	sw	$23,0X0C60($28)
	jal	dely
	sw	$22,0X0C60($28)
	jal	dely
	sw	$21,0XC60($28)
	jal	dely
	sw	$20,0X0C60($28)
	jal	dely
	sw	$19,0X0C60($28)
	jal	dely
	sw	$18,0X0C60($28)
	jal	dely
	sw	$17,0X0C60($28)
	jal	dely
	sw	$18,0X0C60($28)
	jal	dely
	sw	$19,0X0C60($28)
	jal	dely
	sw	$20,0X0C60($28)
	jal	dely
	sw	$21,0X0C60($28)
	jal	dely
	sw	$22,0X0C60($28)
	jal	dely
	sw	$23,0X0C60($28)
	jal	dely
	sw	$24,0X0C60($28)
	jal 	dely
	sw	$25,0X0C60($28)
	lw    	$29,DELAY($0)
lop:	addi	$29,$29,-1
	bne	$29,$0,lop
	j 	writ

