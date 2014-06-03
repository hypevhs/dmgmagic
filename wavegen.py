#/usr/bin/env python

# generate raw bytes for a sin wave with PERIOD (measured in bytes)
# and AMPLITUDE (multiplied to the trig wave).

import sys
import binascii
import math

def main():
	# arguments
	if len(sys.argv) != 4:
		print("usage: [period] [amplitude] [outputfilename]")
		return
	PERIOD = int(sys.argv[1])
	AMPLITUDE = int(sys.argv[2])
	OUTPUTFILENAME = sys.argv[3]
	
	outputBytes = bytearray(PERIOD)
	for index in range(PERIOD):
		angleUnit = index / float(PERIOD)
		angleRads = angleUnit * 2 * math.pi
		waveValue = math.sin(angleRads) * AMPLITUDE
		if waveValue < 0:
			waveValue = waveValue + 256 # two's complement
		outputBytes[index] = int(waveValue)
	print(binascii.hexlify(outputBytes))
	with open(OUTPUTFILENAME, "wb") as outputFile:
		outputFile.write(outputBytes)

main()