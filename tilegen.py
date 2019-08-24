#!/usr/bin/env python3

# convert a png into 8bpp tile data for inclusion

import sys
import png
import binascii

def main():
	if len(sys.argv) == 1:
		print("no png filename specified")
		return
	
	for fileIndex in range(1, len(sys.argv)):
		pngFileName = sys.argv[fileIndex]
		doFile(pngFileName)

def doFile(pngFileName):
	print("readfile: {0}".format(pngFileName))
	pngFile = open(pngFileName, "rb")
	reader = png.Reader(pngFile)
	rgbdata = reader.asRGB8() # boxed row, flat pixel
	
	outputFileName = pngFileName+".2bp"
	outputFile = open(outputFileName, "wb")
	
	width = rgbdata[0]
	height = rgbdata[1]
	if width % 8 != 0 or height % 8 != 0:
		print("bad png: not 8*n by 8*m")
		return
	pixels = list(rgbdata[2])
	
	widthInTiles = width // 8
	heightInTiles = height // 8
	
	for tilesRow in range(heightInTiles):
		for tilesCol in range(widthInTiles):
			# from "pixels", populate "thisPixels"
			thisPixels = []
			gbPixels = []
			
			for rowIndex in range(8):
				y = tilesRow*8+rowIndex
				xstart = tilesCol*(8*3) # 8 pix
				xend = xstart+(8*3) # +8 pix
				thisCol = pixels[y][xstart:xend]
				thisPixels.append(thisCol)
			
			# thisPixels should have 8 row, 8*3 col elements
			assert(len(thisPixels) == 8)
			assert(len(thisPixels[0]) == 8*3)
			
			row = 0
			while row < 8:
				col = 0
				while col < 8:
					pixel = thisPixels[row][col*3:col*3+3]
					r = pixel[0]
					g = pixel[1]
					b = pixel[2]
					if r != g or g != b:
						print("{0} {1} {2}".format(r,g,b))
						print("bad png: color with unequal components" +
							" at x{0},y{1}".format(row + 8*tilesRow, col + 8*tilesCol))
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
				row += 1
			
			# for this 8x8 subtile
			# loop through gbPixels
			howManyBits = 8*8*2
			howManyBytes = howManyBits//8
			outputBytes = bytearray(howManyBytes)
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
			outputFile.write(outputBytes)
			print(binascii.hexlify(outputBytes))
	outputFile.close()

main()
