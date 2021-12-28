        .text                   # Define beginning of text section
        .global _start          # Define entry _start
_start:
        li  x5 , 3               # x5 = 3
        li  x6 , 11              # x6 = 11
        mul x7 , x5 , x6        # x7 = 3 * 11
exit:
        csrw mtohost, 1
        j exit
        .end                    # End of file
