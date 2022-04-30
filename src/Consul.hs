module Consul where
clrscr = "\27[0;0H\27[J"
origin =  "\27[0;0H"
red = "\27[31m"
yellow = "\27[33m"
cian = "\27[36m"
norm = "\27[m"
gray = "\27[1m\27[30m"
rc row col = "\27["++ show row ++";"++ show col ++"f"  -- row,col = 1,2,3...
back n = "\27["++ show n ++"D"
hideCur = "\27[?25l" -- CSI ?25l
showCur = "\27[?25h" -- CSI ?25h