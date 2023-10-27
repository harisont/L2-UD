import sys

with open(sys.argv[1]) as f:
    lines = f.readlines()

newlines = []
for (i,line) in enumerate(lines[1:]):
    if i%2 == 0: 
        newline = line.strip() + "\t"
    else:
        newline = line.strip() + "\n"
    print(newline, end="")