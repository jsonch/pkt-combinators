CC=gcc
CFLAGS=-Wall
LIBS=-lpcap
TARGET=portknock_shared
SRC=portknock_shared.c
INP_JSON=input.json
INP=input.pcap

all: build pcap run

build:
	$(CC) $(CFLAGS) -o $(TARGET) $(SRC) $(LIBS)

pcap: 
	./testutils.py $(INP_JSON) $(INP)

run: build
	./$(TARGET) $(INP)

clean:
	rm -f $(TARGET) $(INP)