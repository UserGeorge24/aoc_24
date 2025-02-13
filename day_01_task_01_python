file_path = "C:\\Users\\szita001\\Desktop\\advent\\2024_12_01_01.txt"

col_1 = []
col_2 = []

with open(file_path,"r") as obj:
    for line in obj:
        temp = line.split(" ")
        col_1.append(int(temp[0]))
        col_2.append(int(temp[3].strip()))

col_1.sort()
col_2.sort()

summa = 0

for index, value in enumerate(col_1):
    summa = summa + abs( value - col_2[index] )
