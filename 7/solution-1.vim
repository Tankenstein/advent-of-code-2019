" Configuration
let s:TAPE_SIZE = 1000

" Operations
let s:ADD = 1
let s:MULTIPLY = 2
let s:INPUT = 3
let s:OUTPUT = 4
let s:JUMP_IF_TRUE = 5
let s:JUMP_IF_FALSE = 6
let s:LESS_THAN = 7
let s:EQUALS = 8
let s:HALT = 99

" Modes
let s:POSITION = 0
let s:IMMEDIATE = 1

function! RunIntCodeFile(fileName, inputs) abort
  let l:rawInput = readfile(a:fileName)[0]
  let l:input = split(l:rawInput, ",")
  let l:numbers = map(l:input, "str2nr(v:val)")
  call s:runIntCode(l:numbers, a:inputs)
endfunction

function! s:runIntCode(tape, inputs) abort
  let l:tape = s:createTape(a:tape, s:TAPE_SIZE)
  let l:inputs = copy(a:inputs)
  let l:index = 0
  let l:inputIndex = 0

  while 1
    let l:operation = l:tape[l:index] % 100

    if l:operation == s:ADD
      let l:firstArgument = s:readArgument(l:tape, l:index, 1)
      let l:secondArgument = s:readArgument(l:tape, l:index, 2)
      let l:valueLocation = s:readPointer(l:tape, l:index, 3)
      let l:tape[l:valueLocation] = l:firstArgument + l:secondArgument
      let l:index += 4
    elseif l:operation == s:MULTIPLY
      let l:firstArgument = s:readArgument(l:tape, l:index, 1)
      let l:secondArgument = s:readArgument(l:tape, l:index, 2)
      let l:valueLocation = s:readPointer(l:tape, l:index, 3)
      let l:tape[l:valueLocation] = l:firstArgument * l:secondArgument
      let l:index += 4
    elseif l:operation == s:INPUT
      let l:firstArgument = s:readPointer(l:tape, l:index, 1)
      let l:tape[l:firstArgument] = l:inputs[l:inputIndex]
      let l:inputIndex += 1
      let l:index += 2
    elseif l:operation == s:OUTPUT
      let l:firstArgument = s:readArgument(l:tape, l:index, 1)
      echo l:firstArgument
      let l:index += 2
    elseif l:operation == s:JUMP_IF_TRUE
      let l:firstArgument = s:readArgument(l:tape, l:index, 1)
      let l:secondArgument = s:readArgument(l:tape, l:index, 2)
      if l:firstArgument != 0
        let l:index = l:secondArgument
      else
        let l:index += 3
      endif
    elseif l:operation == s:JUMP_IF_FALSE
      let l:firstArgument = s:readArgument(l:tape, l:index, 1)
      let l:secondArgument = s:readArgument(l:tape, l:index, 2)
      if l:firstArgument == 0
        let l:index = l:secondArgument
      else
        let l:index += 3
      endif
    elseif l:operation == s:LESS_THAN
      let l:firstArgument = s:readArgument(l:tape, l:index, 1)
      let l:secondArgument = s:readArgument(l:tape, l:index, 2)
      let l:valueLocation = s:readPointer(l:tape, l:index, 3)
      if l:firstArgument < l:secondArgument
        let l:tape[l:valueLocation] = 1
      else
        let l:tape[l:valueLocation] = 0
      endif
      let l:index += 4
    elseif l:operation == s:EQUALS
      let l:firstArgument = s:readArgument(l:tape, l:index, 1)
      let l:secondArgument = s:readArgument(l:tape, l:index, 2)
      let l:valueLocation = s:readPointer(l:tape, l:index, 3)
      if l:firstArgument == l:secondArgument
        let l:tape[l:valueLocation] = 1
      else
        let l:tape[l:valueLocation] = 0
      endif
      let l:index += 4
    elseif l:operation == s:HALT
      return
    else
      throw "ERR: Unknown operation " . l:operation . " used"
    endif
  endwhile
endfunction

function! s:createTape(tape, size) abort
  let l:index = 0
  let l:tape = []
  while l:index < a:size
    let l:tape = add(l:tape, get(a:tape, l:index, 0))
    let l:index += 1
  endwhile
  return l:tape
endfunction

function! s:readArgument(tape, index, offset) abort
  let l:parameterModes = a:tape[a:index] / 100
  let l:value = a:tape[a:index + a:offset]
  let l:mode = l:parameterModes / (s:pow(10, a:offset - 1)) % 10

  if l:mode == s:IMMEDIATE
    return l:value
  elseif l:mode == s:POSITION
    return a:tape[l:value]
  else
    throw "ERR: Unknown parameter mode " . l:mode . " used"
  endif
endfunction

function! s:readPointer(tape, index, offset) abort
  return a:tape[a:index + a:offset]
endfunction

function! s:pow(number, power) abort
  if a:power == 0
    return 1
  endif
  return a:number * s:pow(a:number, a:power - 1)
endfunction
