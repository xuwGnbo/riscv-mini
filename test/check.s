        .text                   # Define beginning of text section
        .global _start          # Define entry _start
_start:
        ADDI x1, x0, 1   # x1 <- 1
        SW   x1, 12(x0)  # Mem[12] <- 1
        LW   x2, 12(x0)  # x2 <- 1
        ADD  x3, x2, x2  # x3 <- 2
        SUB  x4, x2, x3  # x4 <- 1
        SLL  x5, x2, x4  # x5 <- 4
        SLT  x6, x4, x5  # x6 <- 1
        ADD x26,  x0, x1  # x26 <- 1
        ADD x27, x26, x2  # x27 <- 2
        ADD x28, x27, x3  # x28 <- 4
        ADD x29, x28, x4  # x29 <- 5
        ADD x30, x29, x5  # x30 <- 9
        ADD x31, x31, x6  # x31 <- 10
exit:
        csrw mtohost, 1
        j exit
        .end                    # End of file
