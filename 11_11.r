burger = read.table(r"(C:\Users\aes6669ray\Desktop\R Test\data\burger.txt)", header = TRUE, row.names = 1)

pairs(burger,col = "red",gap  = 0 ,xaxt = "n", yaxt =  "n", panel = panel.smooth, col.smooth =  "blue")

