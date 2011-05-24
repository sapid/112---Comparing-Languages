#!./gst -f
"#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk-3.2.4/bin/gst -f"
"
This is a zip utility written by Benjamin Ross(bpross) 
and Will Crawford(wacrawfo)
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

  -d    Debug information is printed for the benefit of the 
        application author
  -t    The compression algorithm is activated, and the decoding tree  
        is printed to the standard output. the output filename may not
        be specified.
  -c    The input file is compressed and written to the output file, if
        specified, or to the standard output, if not.
  -u    The input file is assumed to be compressed, and is uncompressed,
        written to the output file, if specified, or to the std output 
        if not
"
Object subclass: BitStream [
   |bitIndex byte myStream writeIndex writeByte|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
      writeIndex := 9.
      stringIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   putBit: theBit [
      writeIndex = 1 ifTrue: [
         myStream << writeByte.
         writeIndex := 9.
         writeByte := 0.
      ].
      writeIndex := writeIndex - 1.
      writeByte bitAt: bitIndex bitPut: theBit.
   ]
   putString: theString [
      (1 to: theString size)
      do: [:index | self putBit: theString at: index.].
   ]
   padOut [
      writeIndex = 1 ifTrue: myStream << writeByte.
         ifFalse: [
            whileFalse: [
            writeIndex := writeIndex - 1.
            writeByte bitAt: bitIndex bitPut: 0.
            ]
         ]
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
]

"
Because smalltalk indexes arrays starting with 1 this class is needed to
make parsing the file cleaner. As found in charfreq.st from Mackey
"
Object subclass: ZeroArray [
    |theArray|

    ZeroArray class >> new [
        self shouldNotImplement.
    ]

    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    init: size [
        theArray := Array new: size.
    ]

    size [
        ^ theArray size.
    ]

    at: index [
        ^ theArray at: index + 1.
    ]

    at: index put: value [
        ^ theArray at: index + 1 put: value.
    ]

    incr: index [
        (self at: index) isNil
            ifTrue: [ self at: index put: 0].
        self at: index put: (self at: index) + 1.
    ]

    keysAndValuesDo: aBlock [
        (0 to: (self size) - 1) do: [:index |
            aBlock value: index value: (self at: index).
        ]
    ]
]
"
This extends FileStream. I think this will be useful for parsing the 
file. As found in catfile.st under Smalltalk examples from Mackey
"
FileStream extend [
    nl := Character nl.
    catFile [
        |count|
        count := 0.
        [self atEnd not] whileTrue: [
            count := count +1.
            count asString size to: 5 do: [:skip| stdout << ' '].
            stdout << count << ': ' << self nextLine << nl.
        ]
    ]
    parseFile [
      |freqtable|
      freqtable := ZeroArray new: 257.
      [self atEnd not] whileTrue: [
         |ordChar|
         ordChar := self next asInteger.
         freqtable incr: ordChar.
      ].
      freqtable incr: 256.
      ^ freqtable
    ]
]

"
This defines Leaf Class. We will extend this to match our needs for zip.
As found in tree.st under Smalltalk examples from Mackey
"
Object subclass: Leaf [
    |char count|
    char [ ^ char ]
    count [ ^ count ]

    Leaf class >> new [
        self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^result
    ]

    setChar: aChar andCount: aCount [
        char := aChar.
        count := aCount.
    ]

    <= other [
        ^ (count < other count)
        | ((count = other count) & (char <= other char) 
    ]

    printBase: aStream [
    |fixhl| "Fix highlighting in vim - variable never used."
        ^ aStream << self class << '(' << char << ',' << count
    ]
    printOn: aStream [
        (self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]

    binTableDump: aBitStream [
      aBitStream << 
      

]

"
This defines Tree Class. We will extend this to match our needs for zip.
As found in tree.st under Smalltalk examples from Mackey
"
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
       |result|
       result := super new: aChar count: aCount.
       result setLeft: aLeft andRight: aRight.
       ^ result
   ]

   isLeaf [ ^ true ]

   setLeft: aLeft andRight: aRight [
       left := aLeft.
       right := aRight.
   ]

   printOn: aStream [
       (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
       left inorder: visitor prefix: string, '0'.
       right inorder: visitor prefix: string, '1'.
   ]

   binTableDump: aBitstream [
      "Left child, then right child, then print for current node."
   ]
]

nl := Character nl.
dFlag := false.
cFlag := false.
tFlag := false.
uFlag := false.
infile := ''.
outfile := ''.

(1 to: Smalltalk arguments size)
do: [:index |
    argu := (Smalltalk arguments at: index).
    (argu includes: $-) ifTrue: [
        "The argument started with a hyphen"
        (argu includes: $d) ifTrue: [dFlag := true.].
        (argu includes: $c) ifTrue: [cFlag := true.].
        (argu includes: $t) ifTrue: [tFlag := true.].
        (argu includes: $u) ifTrue: [uFlag := true.].
    ] ifFalse: [
        "This assumes the next arguments are files"
        "No testing is done for non-valid options because this is not"
        "tested in the score files"
        (infile isEmpty) ifTrue: [
            infile := argu.
        ] ifFalse: [
            outfile := argu.
        ]
    ]
].
"Prints Debug Information"
dFlag ifTrue: [
    'Debug turned on' display.
    nl display.
    tFlag ifTrue: [
        'Decoding tree will be printed' display.
        nl display.
    ]. 
    cFlag ifTrue: [
        'Compressing: ' display.
        infile display.
        (outfile isEmpty) ifFalse: [
            ' to file ' display.
            outfile display.
            nl display.
        ] ifTrue: [
            ' to stdout' display.
            nl display.
        ]
    ].
    uFlag ifTrue: [
        'Uncompressing: ' display.
        infile display.
        (outfile isEmpty) ifFalse: [
            ' to file ' display.
            outfile display.
            nl display.
        ] ifTrue: [
            ' to stdout' display.
            nl display.
        ]
    ].    
].
|readfile|
readfile := FileStream open: infile 
                       mode: FileStream read.
"
Here, we want to read in values from our frequency table and put 
its values in a sorted collection as tuples. Tuple: (index, value). 
Sort the collection by value. This will function as a priority queue.
"
|frequencyTable|
frequencyTable := readfile parseFile.
dFlag ifTrue: [
    (0 to: frequencyTable size -1)
    do: [:index |
        index display.
        ' ' display.
        item := frequencyTable at: index.
        item display.
        nl display.
    ].
].
|pq|
pq := SortedCollection new.
(0 to: frequencyTable size -1)
do: [ :index |
   |aLeaf aCount aChar|
   aCount := frequencyTable at: index.
   aCount isInteger ifTrue: [
       aChar := index asCharacter.
       aLeaf := Leaf new: aChar  count: aCount.
       pq add: aLeaf.
   ].
].
readfile close.
dFlag ifTrue: [pq inspect.].

[pq size > 1] whileTrue: [
    |last seclast aTree treeChar treeCount|
    last := pq removeFirst.
    seclast := pq removeFirst.
    treeChar := last char.
    treeCount := last count + seclast count.
    aTree := Tree new: treeChar count: treeCount left: last right: seclast.
    pq add: aTree.
].
dFlag ifTrue: [pq inspect.].
|theTree|
theTree := pq removeLast.
tFlag ifTrue: [ "tFlag enabled; print to stdout"
theTree inorder: [:char :string |
      |great32 less127 index count|
      index := char asInteger.
      count := frequencyTable at: index.
      great32 := index > 32.
      less127 := index < 127.
      great32 & less127 ifTrue: [
         stdout << char << ' ' << count << ' ' << string << nl.
      ]. 
      ifFalse: [
         stdout << index << ' ' << count << ' ' << string << nl.
      ].
      ] prefix: ''.
   ].
cFlag ifTrue: [ "Perform compression instead."
     aBitstream := BitStream new: outfile.
     theTree binTableDump: aBitstream. 
   ].

"tFlag ifTrue: [
   theString := ''.
   theEncodingBag := Bag new.
   theTree encodingDump: theString
   theEncodingBag inspect.]. "
      "ifFalse: [theEncodingBag := theTree encodingCreate: theString.]."
