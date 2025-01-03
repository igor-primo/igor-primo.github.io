---
title: Expressive Bash (💥)
subtitle: For those who value ease of reasoning
---

<section>

I was once tasked with creating a minimal Linux distribution
whose sole purpose was to boot and prompt its user with as yes or no
question to whether it should zero all data in the connected
media storage devices. The mission was to give helpdesk professionals
at the company a tool that could be booted from a flash-drive and
automatically ask the user for the instruction, which was to erase
data on the devices so they could be donated.

The strategy I devised was to manually make TinyCore Linux UEFI bootable
and install some common Linux tools (dd, od) along with bash and make
the system start the zero-fill script on boot.

The script I devised was along these lines:
```
#!/bin/bash

BLOCKS=""
BLOCKS_N=""
PROCEED=""
PART=$(df $(pwd) | tail -n 1 | awk '{print $1}' | head -c 8)

sanitize() {
  unset BLOCKS
  unset BLOCKS_N
  unset PROCEED
}

verify_usage() {
  echo -e "\nThe following procedure will erase all data on the media"
  echo "of this machine. Type \"yes\" to proceed."
  read INPUT
  if [ "$INPUT" != "yes" ]
  then
    PROCEED="0"
    return
  else
    echo "THE PROCEDURE WILL ERASE ALL DATA ON THE MACHINE"
    echo "DO YOU REALLY WANT TO PROCEED?"
    echo "IF YES, TYPE \"YES\""
    read INPUT
    if [ "$INPUT" != "YES" ]
    then
      PROCEED="0"
      return
    fi
  fi
  PROCEED="1"
}

get_blocks() {
  BLOCKS_N=($(ls /sys/block/ | grep -v 'ram\|loop'))
  for i in "${BLOCKS_N[@]}"
  do
    if [ "/dev/$i" != "$PART" ]
    then 
      BLOCKS=(${BLOCKS[@]} $i)
    fi
  done
  echo "Working on devices: ${BLOCKS[*]}"
}

generate_commands() {
  for i in "${BLOCKS[@]}"
  do
    COMMAND="dd if=/dev/zero of=/dev/$i bs=5M oflag=direct"
    echo -e "\nWorking on block: $i ($COMMAND)"
    eval "$COMMAND"
    sleep 2
  done
  echo -e "\nTask completed"
}

make_tests() {
  echo "Do you want to perform a zero-fill test on the disks? (\"y\" or \"n\")"
  read TO_TEST
  if [ "$TO_TEST" == "y" ]
  then
    for i in "${BLOCKS[@]}"
    do
      TEST_COMMAND="od /dev/$i"
      echo -e "\nReading the media $i ($TEST_COMMAND)"
      eval "$TEST_COMMAND"
    done
  fi
}

main() {
  while true
  do
    sanitize
    get_blocks
    verify_usage
    if [ "$PROCEED" == "1" ]
    then
      generate_commands
      make_tests
    fi
  done
}

main
```

It is clearly an imperative style script. Now, I have come to gradually
admire the simplicity and power of functional programming; and I think
every one who writes code can benefit from its expressivity. The previous
script has a logical core (the main function) and implementation routines
that do the dirty work
<label for="dirty-work"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="dirty-work"
       class="margin-toggle"/>
<span class="sidenote">
The idea here was to abstract the logic from the implementation.
This is not a nice idea, as often made out to be.
</span>
. This is not nearly enough expressive: function
names do not express how the respective functions *change program state*.
Also, imperative style programming *is not semantically clear*.
Substituting function calls for their implementations makes the program too
*busy* and *overwhelming*, and it doesn't sound like one-lining up everything
could make this thing easier to read:

```
#!/bin/bash

BLOCKS=""
PROCEED=""
PART=$(df $(pwd) | tail -n 1 | awk '{print $1}' | head -c 8)

main() {
  while true
  do
    for i in $(ls /sys/block/ | grep -v 'ram\|loop')
    do
      [ "/dev/$i" != "$PART" ] && BLOCKS=(${BLOCKS[@]} $i)
    done
    echo "Working on devices: ${BLOCKS[*]}"

    echo -e "\nThe following procedure will erase all data on the media"
    echo "of this machine. Type \"yes\" to proceed."
    read FIRST_INPUT
    echo "THE PROCEDURE WILL ERASE ALL DATA ON THE MACHINE"
    echo "DO YOU REALLY WANT TO PROCEED?"
    echo "IF YES, TYPE \"YES\""
    read SECOND_INPUT

    if [ "$FIRST_INPUT" == "yes" ] && [ "$SECOND_INPUT" == "YES" ];
    then
      for i in "${BLOCKS[@]}"
      do
        echo -e "\nWorking on block: $i"
        dd if=/dev/zero of=/dev/"$i" bs=5M oflag=direct && sleep 2
      done
      echo -e "\nTask completed"

      echo "Do you want to perform a zero-fill test on the disks? (\"y\" or \"n\")"
      read TO_TEST
      if [ "$TO_TEST" == "y" ]
      then
        for i in "${BLOCKS[@]}"
        do
          echo -e "\nReading the media $i"
          od /dev/"$i"
        done
      fi
    fi
    unset BLOCKS; unset PROCEED
  done
}

main

```

Suggesting adoption of a functional programming language, though, for most
teams seems like a vain dream, or at least too futuristic. So, instead of
insisting on using a functional language, and if Python cannot be used in this
instance, at least we should seek to make a better use of what we have. I
sought on Internet for people using functional patterns in Bash and I found a
delightful tiny library 
<label for="tiny-library"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="tiny-library"
       class="margin-toggle"/>
<span class="sidenote">
[bash-fun](https://github.com/ssledz/bash-fun)
</span>
that just helps with that.

I decided, then, to rewrite the previous script using functional patterns and
see where I would go.

</section>

<section>

## Rewrite

Let's go through the rewrite step by step.

First, the `sanitize` function can be eradicated,
on account of the fact that `PART` and `BLOCKS`
variables can be made immutable and there is an unset
instruction to unset the needed mutable variables:

```
local -r PART=$(df $(pwd) | tail -n 1 | awk '{print $1}' | head -c 8)
local -r BLOCKS=$(ls /sys/block/ \
  | grep -v 'ram\|loop' \
  | filter lambda a . '[ "/dev/$a" != "$PART" ] && ret true || ret false' \
  | unlist \
  | peek lambda a . echo 'Working on devices $a')
...
unset FIRST_INPUT SECOND_INPUT TO_TEST
```

Second, notice that the `map` function implements the
filtering rule previously implemented by an
imperative loop in the `get_blocks` function,
and `unlist` function makes an
array out of the resulting list, effectively
making the `get_blocks` function obsolete.

Third, the lines
```
echo -e "\nThe following procedure will erase all data on the media"
echo "of this machine. Type \"yes\" to proceed."
read FIRST_INPUT
echo "THE PROCEDURE WILL ERASE ALL DATA ON THE MACHINE"
echo "DO YOU REALLY WANT TO PROCEED?"
echo "IF YES, TYPE \"YES\""
read SECOND_INPUT

if [ "$FIRST_INPUT" == "yes" ] && [ "$SECOND_INPUT" == "YES" ];
then
...
fi
```
can be simplified if we omit the second input, which is unnecessary, given the
warning.
```
echo -e "\nThe following procedure will erase all data on the media of this machine."
echo "THE PROCEDURE WILL ERASE ALL DATA ON THE MACHINE."
read -p "Type \"yes\" to proceed. " FIRST_INPUT
...

[ "$FIRST_INPUT" == "yes" ] \

```

Forth, the lines that do the work can be made more declaratively looking,
if we designate 2 functions to do the imperative effects, `zero` and `testit`.
Then, we pass them to the `map`
<label for="composition"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="composition"
       class="margin-toggle"/>
<span class="sidenote">
Almost composing them.
</span>
that wraps the loop over devices.
```
zero() { 
    echo -e "\nWorking on block /dev/$1" && dd if=/dev/zero of=/dev/"$1" bs=5M oflag=direct
}
testit() {
    echo -e "\nTesting /dev/$1" && od /dev/"$1"
}

[ "$FIRST_INPUT" == "yes" ] \
    && list $(echo "$BLOCKS") \
    | map lambda a . 'zero $a ; sleep 2 && testit $a' \
    && echo -e '\nTask completed'
```
This condenses the logic in the `generate_commands` function.
The test functionality is here enforced, which sounds good
and also simplifies the script.

The final result goes like this:

```
#!/bin/bash

. ~/fun.sh

main () {
  local -r PART=$(df $(pwd) | tail -n 1 | awk '{print $1}' | head -c 8)
  local -r BLOCKS=$(ls /sys/block/ \
    | grep -v 'ram\|loop' \
    | filter lambda a . '[ "/dev/$a" != "$PART" ] && ret true || ret false' \
    | unlist \
    | peek lambda a . echo 'Working on devices $a')

  echo -e "\nThe following procedure will erase all data on the media of this machine."
  echo "THE PROCEDURE WILL ERASE ALL DATA ON THE MACHINE."
  read -p "Type \"yes\" to proceed. " FIRST_INPUT

  zero() { 
    echo -e "\nWorking on block /dev/$1" && dd if=/dev/zero of=/dev/"$1" bs=5M oflag=direct
  }
  testit() {
    echo -e "\nTesting /dev/$1" && od /dev/"$1"
  }

  [ "$FIRST_INPUT" == "yes" ] \
    && list $(echo "$BLOCKS") \
    | map lambda a . 'zero $a ; sleep 2 && testit $a' \
    && echo -e '\nTask completed'

  unset FIRST_INPUT SECOND_INPUT TO_TEST
  main
}

main

```

This version of the script is tiny, very expressive, easy to read, 
and avoids confusion, though it is quite dense, on account of Bash's
exuberant syntax.

But it is still delightful. The meaning of the program and the intention
of the programmer are much clearer and easier to reason about.

I have suspected, for quite some time, that Unix Philosophy and
functional programming somehow marry each other.

</section>
