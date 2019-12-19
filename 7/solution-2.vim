function! FindHighestResult(firmwareFile) abort
  let l:firmware = map(split(readfile(a:firmwareFile)[0], ","), "str2nr(v:val)")
  let l:bestResult = 0

  for l:phaseSettings in s:permutations([5, 6, 7, 8, 9])
    let l:result = s:runAmplifiers(l:phaseSettings, l:firmware)
    if l:result > l:bestResult
      let l:bestResult = l:result
    endif
  endfor

  echo l:bestResult
endfunction

function! s:runAmplifiers(phaseSettings, firmware) abort
  " set up initial machine states with the phase setting, we'll use them later
  let l:index = 0
  let l:machineStates = []
  while l:index < len(a:phaseSettings)
    let l:machineStates = add(l:machineStates, {
      \'state': s:RUNNING,
      \'tape': a:firmware,
      \'index': 0,
      \'inputs': [a:phaseSettings[l:index]],
      \'outputs': [],
    \})
    let l:index += 1
  endwhile

  let l:currentState = 0 " state propagated through the machine
  let l:index = 0
  while 1
    let l:machineState = l:machineStates[l:index]
    let l:machineState.inputs = add(l:machineState.inputs, l:currentState)

    let l:newState = s:runIntCode(l:machineState) " run one output cycle

    if l:newState.state == s:HALTED && l:index == len(a:phaseSettings) - 1 " last machine halts
      break
    endif

    if len(l:newState.outputs) " if a machine halts, it has no outputs.
      let l:output = l:newState.outputs[0]
      let l:newState.outputs = []

      let l:machineStates[l:index] = l:newState
      let l:currentState = l:output
    endif

    let l:index += 1
    let l:index = l:index % len(a:phaseSettings) " keep going back to the first one
  endwhile

  return l:currentState
endfunction

function! s:permutations(list) abort
  if len(a:list) == 0
    return []
  elseif len(a:list) == 1
    return [a:list]
  endif

  let l:permutations = []
  let l:index = 0
  while l:index < len(a:list)
    let l:element = a:list[l:index]
    let l:rest = filter(copy(a:list), "v:val != l:element")

    for l:permutation in s:permutations(l:rest)
      let l:permutations = add(l:permutations, [l:element] + l:permutation)
    endfor

    let l:index += 1
  endwhile

  return l:permutations
endfunction

" Int machine code starts here -----------------------

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

" Machine states
let s:RUNNING = 0
let s:HALTED = 1
let s:YIELD = 2

" settings should contain tape, index, outputs and inputs
function! s:runIntCode(settings) abort
  let l:tape = s:createTape(a:settings.tape, s:TAPE_SIZE)
  let l:inputs = copy(a:settings.inputs)
  let l:outputs = copy(a:settings.outputs)
  let l:index = a:settings.index

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
      let l:tape[l:firstArgument] = l:inputs[0]
      call remove(l:inputs, 0)
      let l:index += 2
    elseif l:operation == s:OUTPUT
      let l:firstArgument = s:readArgument(l:tape, l:index, 1)
      let l:outputs = add(l:outputs, l:firstArgument)
      let l:index += 2
      return {
        \'state': s:YIELD,
        \'tape': l:tape,
        \'index': l:index,
        \'inputs': l:inputs,
        \'outputs': l:outputs,
      \}
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
      return {
        \'state': s:HALTED,
        \'tape': l:tape,
        \'index': l:index,
        \'inputs': l:inputs,
        \'outputs': l:outputs,
      \}
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

function! s:pow(number, power) abort
  if a:power == 0
    return 1
  endif
  return a:number * s:pow(a:number, a:power - 1)
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

