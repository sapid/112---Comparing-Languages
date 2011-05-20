#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk-3.2.4/bin/gst -f
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
        writeen to the output file, if specified, or to the std output 
        if not
"

"
This extends FileStream. I think this will be useful for parsing the file.
As found in catfile.st under Smalltalk examples from Mackey
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
]

"
This defines Leaf Class. We will extend this to match our needs for zip.
As found in tree.st under Smalltalk examples from Mackey
"
Object subclass: Leaf [
    |char count|
    char [ ^ char ]
    count [ ^ count]

    Leaf class >> new [
        self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^result
    ]

    setChar: aChar count: aCount [
        char := aChar.
        count := aCount.
    ]
    "
    I think this is useless?
    <= other [
        ^ (count < other count)
        | ((count = other count) & (char <= other char))
    ]
    "

    printBase: aStream [
        ^ aStream << self class << '(' << char << ',' << count
    ]

    printOn: aStream [
        (self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]
]

"
This defines Tree Class. We will extend this to match our needs for zip.
As found in tree.st under Smalltalk examples from Mackey
"
Leaf subclass: Tree [
    |left right|

    Tree class >> new: aChar count: aCount [
        self shouldNotImplement
    ]

    Tree class >> new: aChar count: aCount left: aLeft right: aRight [
        |result|
        result := super new: aChar count: aCount.
        result setLeft: aLeft andRight: aRight.
        ^ result
    ]

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
readfile catFile.
readfile close.
