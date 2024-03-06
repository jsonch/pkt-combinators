CC=gcc
NUM_CORES=3
CFLAGS=-Wall -DNUM_CORES=$(NUM_CORES)
LIBS=-lpcap
TARGET=portknock_scr
SRC=portknock_scr.c
INP_JSON=input.json
INP=input.pcap
SCRPCAP=scr_$(NUM_CORES).pcap

all: build pcap run

# the convertoscrpcap command, which is a python
# script that converts the original pcap into
# a scr pcap (by adding the dummy ethernet header
# and packet history)
GENSCRPAP=python3 converttoscrpcap.py -i $(INP) -o . -n $(NUM_CORES)
# the splitpcap command, which is a python
# script that splits the pcap and also 
# prints the nams of the split files
# SPLIT=`./splitpcap.py $(INP) 3`
SPLIT=`python3 splitpcap.py $(SCRPCAP) $(NUM_CORES)`

build:
	$(CC) $(CFLAGS) -o $(TARGET) $(SRC) $(LIBS)

pcap: 
	./makepcap.py $(INP_JSON) $(INP)

run: build
	$(GENSCRPAP)
	./$(TARGET) $(SPLIT)

clean:
	rm -f $(TARGET) $(INP) $(SCRPCAP).*
