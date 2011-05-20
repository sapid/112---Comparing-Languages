#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk-3.2.4/bin/gst
"
This is a zip utility written by Benjamin Ross(bpross) and Will Crawford(wacrawfo)
for CMPS 112 at UCSC.
Date: May 18, 2011
Uses the Huffman code to zip files
"

"
USAGE:
hzip.st -dtcu inputfile [outfile]
"

"
Options:
Exactly one of these options is required

  -d    Debug information is printed for the benefit of the application author
  -t    The compression algorithm is activated, and the decoding tree is 
        printed to the standard output. the output filename may not be
        specified.
  -c    The input file is compressed and written to the output file, if
        specified, or to the standard output, if not.
  -u    The input file is assumed to be compressed, and is uncompressed,
        writeen to the output file, if specified, or to the std output if not
"


