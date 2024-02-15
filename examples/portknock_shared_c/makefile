CC=gcc
CFLAGS=-Wall
LIBS=-lpcap
TARGET=portknock_shared
SRC=portknock_shared.c
INP_JSON=input.json
INP=input.pcap

# the splitpcap command, which is a python
# script that splits the pcap and also 
# prints the nams of the split files
SPLIT=`./splitpcap.py $(INP) 3`

all: build pcap run

build:
	$(CC) $(CFLAGS) -o $(TARGET) $(SRC) $(LIBS)

pcap: 
	./makepcap.py $(INP_JSON) $(INP)

run: build
	./$(TARGET) $(SPLIT)

clean:
	rm -f $(TARGET) $(INP)