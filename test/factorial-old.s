
factorial-old:     file format elf32-littleriscv


Disassembly of section .text:

00000200 <main>:
 200:	fe010113          	addi	sp,sp,-32
 204:	00112e23          	sw	ra,28(sp)
 208:	00812c23          	sw	s0,24(sp)
 20c:	02010413          	addi	s0,sp,32
 210:	00a00513          	li	a0,10
 214:	00c000ef          	jal	220 <factorial>
 218:	fea42623          	sw	a0,-20(s0)
 21c:	064000ef          	jal	280 <exit>

00000220 <factorial>:
 220:	fe010113          	addi	sp,sp,-32
 224:	00112e23          	sw	ra,28(sp)
 228:	00812c23          	sw	s0,24(sp)
 22c:	02010413          	addi	s0,sp,32
 230:	fea42623          	sw	a0,-20(s0)
 234:	fec42703          	lw	a4,-20(s0)
 238:	00100793          	li	a5,1
 23c:	00e7c663          	blt	a5,a4,248 <factorial+0x28>
 240:	00100793          	li	a5,1
 244:	0280006f          	j	26c <factorial+0x4c>
 248:	fec42783          	lw	a5,-20(s0)
 24c:	fff78793          	addi	a5,a5,-1
 250:	00078513          	mv	a0,a5
 254:	fcdff0ef          	jal	220 <factorial>
 258:	00050793          	mv	a5,a0
 25c:	fec42583          	lw	a1,-20(s0)
 260:	00078513          	mv	a0,a5
 264:	030000ef          	jal	294 <__mulsi3>
 268:	00050793          	mv	a5,a0
 26c:	00078513          	mv	a0,a5
 270:	01c12083          	lw	ra,28(sp)
 274:	01812403          	lw	s0,24(sp)
 278:	02010113          	addi	sp,sp,32
 27c:	00008067          	ret

00000280 <exit>:
 280:	ff010113          	addi	sp,sp,-16
 284:	00812623          	sw	s0,12(sp)
 288:	01010413          	addi	s0,sp,16
 28c:	7800d073          	csrwi	mtohost,1
 290:	ffdff06f          	j	28c <exit+0xc>

00000294 <__mulsi3>:
 294:	00050613          	mv	a2,a0
 298:	00000513          	li	a0,0
 29c:	0015f693          	andi	a3,a1,1
 2a0:	00068463          	beqz	a3,2a8 <__mulsi3+0x14>
 2a4:	00c50533          	add	a0,a0,a2
 2a8:	0015d593          	srli	a1,a1,0x1
 2ac:	00161613          	slli	a2,a2,0x1
 2b0:	fe0596e3          	bnez	a1,29c <__mulsi3+0x8>
 2b4:	00008067          	ret
