    .text
    .global _start
_start:
    li x1, 0x10000000       # read valid
    li x2, 0x10000004       # read data
    li x3, 0x10000008       # write ready
    li x4, 0x1000000C       # write data

ECHO_LOOP:
CheckReadValid:
    lb x5, 0(x1)
    beq x5, x0, CheckReadValid
    lb x6, 0(x2)                #if valid == 1 read data else loop
CheckWriteReady:
    lb x5,0(x3)
    beq x5,x0,CheckWriteReady   #if rady == 1 write data else loop
    sb x6,0(x4)
    j ECHO_LOOP
