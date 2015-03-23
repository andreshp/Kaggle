#!/usr/bin/python

######################################################################
# Autor: Andres Herrera Poyatos
# Universidad de Granada, March, 2015
# Enssamble Titanic
#######################################################################

import sys             # sys.argv
import time            # time para medir el tiempo

######################## MAIN ##########################

# Comprobacion de los argumentos
if len(sys.argv) < 3:
    print("Sintaxis: python3 enssamble.py <firs.csv> <second.csv> <other.csv...>")
    sys.exit()

enssamble = open("enssamble1.csv", "w")

# Llamada a la funcion
start_time = time.time()
files = []
for i in range(1, len(sys.argv)):
    f = open(sys.argv[i])
    f.readline()
    data = []
    for line in f:
        data.append(int(line.split(sep=",")[1].split(sep="\"")[1]))
    files.append(data)

f = open(sys.argv[1])
f.readline()

enssamble.write("\"PassengerId\",\"Survived\"\n")
j = 0
for line in f:
    index = line.split(sep=",")[0]
    sum_ = 0
    for i in range(0, len(sys.argv)-1):
        sum_ += files[i][j]
    if sum_ >= 0.5*(len(sys.argv)-1):
        sum_ = 1
    else:
        sum_ = 0
    enssamble.write(index+","+str(sum_)+"\n")
    j += 1

print("--- %f segundos ---" % (time.time() - start_time) )
print("Enssamble obtained.")

