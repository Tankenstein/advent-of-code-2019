#!/usr/bin/env bash
input_file="$1"
set -u

# program codes
ADD=1
MULTIPLY=2
INPUT=3
OUTPUT=4
JUMP_IF_TRUE=5
JUMP_IF_FALSE=6
LESS_THAN=7
EQUALS=8
HALT=99

# program modes
POSITION=0
IMMEDIATE=1

if [ "$input_file" == "" ]
then
    echo "NO PROGRAM FILE PROVIDED"
    exit 1
fi

IFS=',' read -r -a tape <<< $(cat $input_file)

inputs=(5)

function run_intmachine() {
    local index=0

    while [ true ]
    do
        local tape_value=${tape[$index]}
        local operation=$(($tape_value%100))
        local parameter_modes=$(($tape_value/100))

        case "$operation"
        in
            "$ADD")
                local arg_1=${tape[$index + 1]}
                local arg_1_mode=$((parameter_modes % 10))
                local arg_1_val=$([ "$arg_1_mode" -eq $IMMEDIATE ] && echo "$arg_1" || echo "${tape[$arg_1]}")

                local arg_2=${tape[$index + 2]}
                local arg_2_mode=$((parameter_modes / 10 % 10))
                local arg_2_val=$([ "$arg_2_mode" -eq $IMMEDIATE ] && echo "$arg_2" || echo "${tape[$arg_2]}")
                local value_position=${tape[$index + 3]} # Will never be in immediate mode.

                local sum=$(($arg_1_val + $arg_2_val))
                tape[$value_position]=$sum

                index=$(($index + 4))
            ;;
            "$MULTIPLY")
                local arg_1=${tape[$index + 1]}
                local arg_1_mode=$((parameter_modes % 10))
                local arg_1_val=$([ "$arg_1_mode" -eq $IMMEDIATE ] && echo "$arg_1" || echo "${tape[$arg_1]}")

                local arg_2=${tape[$index + 2]}
                local arg_2_mode=$((parameter_modes / 10 % 10))
                local arg_2_val=$([ "$arg_2_mode" -eq $IMMEDIATE ] && echo "$arg_2" || echo "${tape[$arg_2]}")
                local value_position=${tape[$index + 3]} # Will never be in immediate mode.

                local result=$(($arg_1_val * $arg_2_val))
                tape[$value_position]=$result

                index=$(($index + 4))
            ;;
            "$INPUT")
                local arg_1=${tape[$index + 1]} # Will never be in immediate mode.

                tape[$arg_1]=${inputs[0]}
                inputs=(${inputs:1}) # remove an element

                index=$(($index + 2))
            ;;
            "$OUTPUT")
                local arg_1=${tape[$index + 1]}
                local arg_1_mode=$((parameter_modes % 10))
                local arg_1_val=$([ "$arg_1_mode" -eq $IMMEDIATE ] && echo "$arg_1" || echo "${tape[$arg_1]}")

                printf "$arg_1_val\n"

                index=$(($index + 2))
            ;;
            "$JUMP_IF_TRUE")
                local arg_1=${tape[$index + 1]}
                local arg_1_mode=$((parameter_modes % 10))
                local arg_1_val=$([ "$arg_1_mode" -eq $IMMEDIATE ] && echo "$arg_1" || echo "${tape[$arg_1]}")

                local arg_2=${tape[$index + 2]}
                local arg_2_mode=$((parameter_modes / 10 % 10))
                local arg_2_val=$([ "$arg_2_mode" -eq $IMMEDIATE ] && echo "$arg_2" || echo "${tape[$arg_2]}")
                if [ $arg_1_val -ne 0 ]
                then
                    index=$arg_2_val
                else
                    index=$(($index + 3))
                fi
            ;;
            "$JUMP_IF_FALSE")
                local arg_1=${tape[$index + 1]}
                local arg_1_mode=$((parameter_modes % 10))
                local arg_1_val=$([ "$arg_1_mode" -eq $IMMEDIATE ] && echo "$arg_1" || echo "${tape[$arg_1]}")

                local arg_2=${tape[$index + 2]}
                local arg_2_mode=$((parameter_modes / 10 % 10))
                local arg_2_val=$([ "$arg_2_mode" -eq $IMMEDIATE ] && echo "$arg_2" || echo "${tape[$arg_2]}")

                if [ $arg_1_val -eq 0 ]
                then
                    index=$arg_2_val
                else
                    index=$(($index + 3))
                fi
            ;;
            "$LESS_THAN")
                local arg_1=${tape[$index + 1]}
                local arg_1_mode=$((parameter_modes % 10))
                local arg_1_val=$([ "$arg_1_mode" -eq $IMMEDIATE ] && echo "$arg_1" || echo "${tape[$arg_1]}")

                local arg_2=${tape[$index + 2]}
                local arg_2_mode=$((parameter_modes / 10 % 10))
                local arg_2_val=$([ "$arg_2_mode" -eq $IMMEDIATE ] && echo "$arg_2" || echo "${tape[$arg_2]}")

                local value_position=${tape[$index + 3]} # Will never be in immediate mode.

                if [ $arg_1_val -lt $arg_2_val ]
                then
                    tape[$value_position]=1
                else
                    tape[$value_position]=0
                fi

                index=$(($index + 4))
            ;;
            "$EQUALS")
                local arg_1=${tape[$index + 1]}
                local arg_1_mode=$((parameter_modes % 10))
                local arg_1_val=$([ "$arg_1_mode" -eq $IMMEDIATE ] && echo "$arg_1" || echo "${tape[$arg_1]}")

                local arg_2=${tape[$index + 2]}
                local arg_2_mode=$((parameter_modes / 10 % 10))
                local arg_2_val=$([ "$arg_2_mode" -eq $IMMEDIATE ] && echo "$arg_2" || echo "${tape[$arg_2]}")

                local value_position=${tape[$index + 3]} # Will never be in immediate mode.

                if [ $arg_1_val -eq $arg_2_val ]
                then
                    tape[$value_position]=1
                else
                    tape[$value_position]=0
                fi

                index=$(($index + 4))
            ;;
            "$HALT")
                printf "PROGRAM HALTED\n"
                break
            ;;
            *)
                printf "UNKNOWN OPERATION $operation\n"
                exit 1
            ;;
        esac
    done
}

run_intmachine
