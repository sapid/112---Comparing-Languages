#!./gst -f

FileStream extend [
   |bitIndex currentChar writeByte writeIndex|

   nextBit [
      |bit|
      bitIndex isNil ifTrue: [bitIndex := 0].
      bitIndex = 0 ifTrue: [
         bitIndex := 8.
         currentChar := self next.
      ].
      bit := currentChar value bitAt: bitIndex.
      bitIndex := bitIndex - 1.
      ^ bit
   ]

   putBit: theBit [
      writeIndex isNil ifTrue: [ writeIndex := 9 ].
      writeByte isNil ifTrue: [ writeByte := 0 ].
      writeIndex := writeIndex - 1.
      writeByte bitAt: writeIndex put: theBit.
      writeIndex = 0 ifTrue: [
         self << writeByte.
         writeByte := 9.
      ].
   ]
   putString: theString [
      (1 to: theString size -1)
      do: [:index | self putBit: theString at: index.].
   ]
   padOut [
      writeIndex isNil ifFalse: [
         writeIndex = 9 ifFalse: [
            writeIndex = 1 whileFalse: [
               self putBit: 0.
               ].].]
   ]

   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]

]

nl := Character nl.
[stdin atBitEnd not] whileTrue: [
   stdout putBit: (stdin nextBit).
]
stdout padOut.

