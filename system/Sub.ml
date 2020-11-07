type 'msg onesub =  
  | SubTick of (Time.posix -> 'msg)
  | SubMouseMove of (float * float -> 'msg)
  | SubMouseDown of (unit -> 'msg)
  | SubMouseUp of (unit -> 'msg)
  | SubKeyDown of (Keyboard.key -> 'msg)
  | SubKeyUp of (Keyboard.key -> 'msg)


type 'msg t = 'msg onesub list
let none = []
let batch xs = (List.flatten xs)
