#!/usr/bin/env python

# convert a 8x8 png into 8bpp tile data for inclusion

import sys
import png
import binascii

def main():
	if len(sys.argv) != 2:
		print("no png filename specified")
		return
	pngFileName = sys.argv[1]
	print("readfile: {0}".format(pngFileName))
	pngFile = open(pngFileName, "rb")
	reader = png.Reader(pngFile)
	rgbIter = reader.asRGB8()
	if rgbIter[0] != 8 or rgbIter[1] != 8:
		print("bad png: not 8x8")
		return
	pixels = rgbIter[2]
	gbPixels = []
	for row in pixels:
		col = 0
		while col < 8:
			index = col*3
			r = row[index]
			g = row[index+1]
			b = row[index+2]
			if r != g or g != b:
				print("bad png: color with unequal components")
				return
			byteVal = r # could be g or b, but they're eq in grayscale
			if byteVal == 0:
				gbVal = 3
			elif byteVal == 104:
				gbVal = 2
			elif byteVal == 176:
				gbVal = 1
			elif byteVal == 255:
				gbVal = 0
			else:
				print("bad png: bad color")
				return
			gbPixels.append(gbVal)
			col += 1
	#close(pngFile)
	# loop through gbPixels
	outputBytes = bytearray(16)
	row = 0
	while row < 8:
		loByte = 0
		hiByte = 0
		col = 0
		while col < 8:
			loByte <<= 1
			hiByte <<= 1
			thisGb = gbPixels[row*8+col]
			if thisGb == 1 or thisGb == 3:
				loByte += 1
			if thisGb == 2 or thisGb == 3:
				hiByte += 1
			col += 1
		outputBytes[row*2] = loByte
		outputBytes[row*2+1] = hiByte
		row += 1
	print(binascii.hexlify(outputBytes))
	outputFileName = pngFileName+".2bp"
	with open(outputFileName, "wb") as outputFile:
		outputFile.write(outputBytes)

main()

#import png
#pngFile=open("supera.png","rb")
#reader=png.Reader(pngFile)
#rgb = reader.asRGB8()
#pixMeta = rgb[2]
#for row in pix:
#	print(str(row))