#!/usr/bin/python

import struct
import StringIO

def main():
	import sys
	print("Reading {0}".format(sys.argv[1]))
	parseModule(sys.argv[1])

def parseModule(fileName):
	"""
	http://www.delek.com.ar/soft/deflemask/DMF_SPECS.txt
	http://www.delek.com.ar/soft/deflemask/DMP_SPECS.txt
	"""
	data = StringIO.StringIO(openAndDecompress(fileName))

	decoded = {}

	#### read all the data ####

	## format flags
	# magic, version
	assert(data.read(16) == ".DelekDefleMask.")
	decoded["version"] = ord(data.read(1))

	## system set
	# system
	rawSystem = ord(data.read(1))
	assert(1 <= rawSystem <= 7)
	decoded["system"] = rawSystem
	systemTotalChannelsDecoder = [None,17,10,4,4,6,5,3]
	decoded["systemTotalChannels"] = systemTotalChannelsDecoder[rawSystem]

	## visual information
	# name, author, highlights
	rawSongNameLength = ord(data.read(1))
	decoded["songName"] = data.read(rawSongNameLength)
	rawSongAuthorLength = ord(data.read(1))
	decoded["songAuthor"] = data.read(rawSongAuthorLength)
	decoded["highlightA"] = ord(data.read(1))
	decoded["highlightB"] = ord(data.read(1))

	## TODO: module information
	# timing, PAL/NTSC, row counts, arpeggio tick
	data.read(8)
	decoded["totalRowsPattern"] = ord(data.read(1))
	decoded["totalRowsMatrix"] = ord(data.read(1))
	decoded["arpeggioTickSpeed"] = ord(data.read(1))

	## TODO: pattern matrix values
	for chan in range(decoded["systemTotalChannels"]):
		for patt in range(decoded["totalRowsMatrix"]):
			data.read(1)

	## instruments data
	rawInstrumentsLength = ord(data.read(1))
	decoded["instruments"] = [None] * rawInstrumentsLength
	for instr in range(rawInstrumentsLength):
		decoded["instruments"][instr] = {}
		thisInstr = decoded["instruments"][instr]
		rawInstrNameLength = ord(data.read(1))
		thisInstr["name"] = data.read(rawInstrNameLength)
		thisInstr["isFm"] = ord(data.read(1)) == 1
		if not thisInstr["isFm"]:
			# volume macro
			rawInstrMacroLength = ord(data.read(1))
			thisInstr["macroVolume"] = [None] * rawInstrMacroLength
			for mac in range(rawInstrMacroLength):
				thisInstr["macroVolume"][mac] = toU32(data.read(4))
				# toU16(data.read(2)) [0, 0, 8, 0, 7, 0, 6, 0, 6]
				# ord(data.read(1))   [0, 0, 0, 0, 8, 0, 0, 0, 7]
			if (rawInstrMacroLength != 0): # the documentation never specified this special case
				thisInstr["macroVolumeLoop"] = toS8(data.read(1))

			# arpeggio macro
			rawInstrMacroLength = ord(data.read(1))
			thisInstr["macroArp"] = [None] * rawInstrMacroLength
			for mac in range(rawInstrMacroLength):
				thisInstr["macroArp"][mac] = toS32(data.read(4))
			if (rawInstrMacroLength != 0):
				thisInstr["macroArpLoop"] = toS8(data.read(1))
			thisInstr["macroArpIsFixed"] = ord(data.read(1)) == 1
			#{'name': 'bass', ... 'macroArp': [12, 24, 12], ... 'macroArpIsFixed': False}
			#{'name': 'bass', ... 'macroArp': [12, 24, 12], ... 'macroArpIsFixed': True}
			if (not thisInstr["macroArpIsFixed"] and "macroArpLoop" in thisInstr):
				thisInstr["macroArpLoop"] -= 12

			# duty noise macro
			rawInstrMacroLength = ord(data.read(1))
			thisInstr["macroDuty"] = [None] * rawInstrMacroLength
			for mac in range(rawInstrMacroLength):
				thisInstr["macroDuty"][mac] = toU32(data.read(4))
			if (rawInstrMacroLength != 0):
				thisInstr["macroDutyLoop"] = toS8(data.read(1))

			# wavetable macro
			rawInstrMacroLength = ord(data.read(1))
			thisInstr["macroWave"] = [None] * rawInstrMacroLength
			for mac in range(rawInstrMacroLength):
				thisInstr["macroWave"][mac] = toU32(data.read(4))
			if (rawInstrMacroLength != 0):
				thisInstr["macroWaveLoop"] = toS8(data.read(1))
		else:
			print("FM not supported yet.") #TODO
			pass

	## end of instruments data
	## wavetables data
	rawAllWavetableLength = ord(data.read(1))
	decoded["wavetables"] = [None] * rawAllWavetableLength
	for wave in range(rawAllWavetableLength):
		rawWavetableLength = toU32(data.read(4))
		decoded["wavetables"][wave] = [None] * rawWavetableLength
		for sample in range(rawWavetableLength):
			decoded["wavetables"][wave][sample] = toU32(data.read(4))

	## patterns data
	#decoded["music"][0][1][2]["note"]
	#...chan 0, patt 1, row 2
	#consider tuples; lots of reused keys.
	decoded["music"] = [None] * decoded["systemTotalChannels"]
	for chan in range(decoded["systemTotalChannels"]):
		decoded["music"][chan] = [None] * decoded["totalRowsMatrix"]
		rawChanEffectsLength = ord(data.read(1))
		for patt in range(decoded["totalRowsMatrix"]):
			decoded["music"][chan][patt] = [None] * decoded["totalRowsPattern"]
			for row in range(decoded["totalRowsPattern"]):
				decoded["music"][chan][patt][row] = {}
				decoded["music"][chan][patt][row]["note"] = toS16(data.read(2)) # Note
				decoded["music"][chan][patt][row]["octave"] = toS16(data.read(2)) # Octave
				decoded["music"][chan][patt][row]["volume"] = toS16(data.read(2)) # Volume
				decoded["music"][chan][patt][row]["effect"] = [None] * rawChanEffectsLength
				for fx in range(rawChanEffectsLength):
					decoded["music"][chan][patt][row]["effect"][fx] = {}
					decoded["music"][chan][patt][row]["effect"][fx]["code"] = toS16(data.read(2)) # Effect code
					decoded["music"][chan][patt][row]["effect"][fx]["value"] = toS16(data.read(2)) # Effect value
				decoded["music"][chan][patt][row]["instrument"] = toS16(data.read(2)) # Instrument

	## pcm samples data
	rawTotalSamples = ord(data.read(1))
	decoded["samples"] = [None] * rawTotalSamples
	for samp in range(rawTotalSamples):
		decoded["samples"][samp] = {}
		rawSampleSize = toU32(data.read(4))
		decoded["samples"][samp]["rate"] = data.read(1)
		decoded["samples"][samp]["pitch"] = data.read(1)
		decoded["samples"][samp]["amp"] = data.read(1)
		decoded["samples"][samp]["data"] = [None] * rawSampleSize
		for sampdata in range(rawSampleSize):
			decoded["samples"][samp]["data"][sampdata] = toS16(data.read(2))

	#import pprint; pp = pprint.PrettyPrinter(indent=4); pp.pprint(decoded)
	return decoded

def openAndDecompress(fileName):
	import zlib
	with open(fileName, "rb") as fi:
		raw = fi.read()
	dec = zlib.decompress(raw)
	return dec

def toU32(bits):
	return struct.unpack_from("<I", bits)[0]

def toS32(bits):
	return struct.unpack_from("<i", bits)[0]

def toU16(bits):
	return struct.unpack_from("<H", bits)[0]

def toS16(bits):
	return struct.unpack_from("<h", bits)[0]

def toS8(bits):
	return struct.unpack_from("b", bits)[0]

if __name__ == "__main__":
	main()
