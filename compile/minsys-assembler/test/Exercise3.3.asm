.DATA  0x0000
   a:   .word 7
.TEXT  0x0000
start:
	lui   $1,0XFFFF				#;让$28为0FFFF0000H作为端口地址的高16位
        ori   $28,$1,0XF000			#;$28端口是系统的I/O地址的高20位
read:	lw   $1,0XC70($28)		#;从拨码开关读取数据
	and $s3,$s3,$0
	and $s4,$s4,$0
	andi $s3,$1,0XF000
	srl    $s3,$s3,12		       	#$s3为被乘数 SW15~SW12
	andi $s4,$1,0X000F  		#$s4为乘数  SW3~SW0
	ori $s8,$zero,0		            	#计算结果放在$s8中 LD7~LD0
	ori $s5,$zero,1
beg:
	and $s6,$s5,$s4			#获取$s4的最低位数值
	srl  $s4,$s4,1				#将$s4的次低位变为最低位
	beq $s6,$zero,next              	#$s4的最低位是0，不用加上被乘数
	add $s8,$s8,$s3			#加上被乘数
next:
	sll $s3,$s3,1                       	#将被乘数乘以2
	beq $s4,$zero,exit               	#乘数已经变为0，计算完毕
	j beg
exit:
	sw   $s8,0X0C60($28)		#送LED显示
	j read

