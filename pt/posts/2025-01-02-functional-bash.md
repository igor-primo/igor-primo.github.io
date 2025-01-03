---
title: Bash Expressivo (💥)
subtitle: Para aqueles que valorizam facilidade de raciocínio
---

<section>

Uma vez fui encarregado de criar uma distribuição Linux mínima cujo único
propósito era inicializar e apresentar ao usuário uma pergunta de sim ou não
sobre se todos os dados nos dispositivos de armazenamento conectados deveriam
ser zerados. A missão era fornecer aos profissionais de suporte técnico da
empresa uma ferramenta que pudesse ser inicializada a partir de um pendrive e,
automaticamente, solicitasse ao usuário a instrução de apagar os dados nos
dispositivos, para que os dispositivos pudessem ser doados.

A estratégia que desenvolvi foi manualmente tornar o TinyCore Linux
inicializável via UEFI, instalar algumas ferramentas Linux comuns (dd, od)
junto com o bash, e configurar o sistema para iniciar o script de preenchimento
com zeros na inicialização.

O script
<label for="script"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="script"
       class="margin-toggle"/>
<span class="sidenote">
O script original obviamente possuía strings em português,
mas a versão original do post é em inglês.
</span>
que criei era algo parecido com isso: 

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

É claramente um script de estilo imperativo. Agora, comecei a admirar 
gradualmente a simplicidade e o poder da programação funcional; e acredito 
que todos que escrevem código podem se beneficiar de sua expressividade. 
O script anterior possui um núcleo lógico (a função principal) e rotinas de 
implementação que fazem o trabalho pesado
<label for="dirty-work"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="dirty-work"
       class="margin-toggle"/>
<span class="sidenote">
A ideia aqui era abstrair a lógica da implementação. 
Isso não é exatamente uma boa ideia, como muitas vezes se faz parecer.
</span>
. Isso não é nem de longe suficientemente expressivo: os nomes das funções não
expressam como as respectivas funções *alteram o estado do programa*. Além
disso, a programação em estilo imperativo *não é semanticamente clara*.
Substituir chamadas de funções por suas implementações torna o programa muito
*carregado* e *opressivo*, e não parece que colocar vários comandos em uma
única linha tornaria isso mais fácil de ler:

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

Sugerir a adoção de uma linguagem de programação funcional, no entanto, para a
maioria das equipes, parece um sonho vão ou, no mínimo, muito futurista. Então,
em vez de insistir no uso de uma linguagem funcional, e caso Python não possa
ser usado em um caso particular, pelo menos devemos buscar um melhor
aproveitamento do que temos. Procurei na Internet por pessoas utilizando
padrões funcionais em Bash e encontrei uma pequena biblioteca encantadora
<label for="tiny-library"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="tiny-library"
       class="margin-toggle"/>
<span class="sidenote">
[bash-fun](https://github.com/ssledz/bash-fun)
</span>
que ajuda justamente nisso.

Decidi, então, reescrever o script anterior utilizando padrões funcionais 
e ver aonde isso me levaria.

</section>

<section>

## Reescrita

Vamos passar pela reescrita passo a passo.

Primeiro, a função `sanitize` pode ser eliminada, pelo fato de que as variáveis
`PART` e `BLOCKS` podem ser feitas imutáveis e há uma instrução `unset` para
limpar as variáveis mutáveis necessárias:

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

Segundo, observe que a função `map` implementa a regra de filtragem que
anteriormente era realizada por um loop imperativo na função `get_blocks`, e a
função `unlist` transforma a lista resultante em um array, tornando a função
`get_blocks` obsoleta.

Terceiro, as linhas
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
podem ser simplificadas se omitirmos a segunda entrada, que é desnecessária, 
dado o aviso veemente.
```
echo -e "\nThe following procedure will erase all data on the media of this machine."
echo "THE PROCEDURE WILL ERASE ALL DATA ON THE MACHINE."
read -p "Type \"yes\" to proceed. " FIRST_INPUT
...

[ "$FIRST_INPUT" == "yes" ] \

```

Quarto, as linhas que fazem o trabalho podem ser tornadas mais declarativas, 
se designarmos 2 funções para realizar os efeitos imperativos, `zero` e `testit`. 
Em seguida, as passamos para o `map`
<label for="composicao"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="composicao"
       class="margin-toggle"/>
<span class="sidenote">
Quase como compondo-as.
</span>
, que "faz" o loop sobre os dispositivos.
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
Isso condensa a lógica na função `generate_commands`. A funcionalidade de teste
é aqui reforçada, o que parece bom e também simplifica o script.

O resultado final fica assim:

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

Esta versão do script é bem pequena, muito expressiva, fácil de ler e evita
confusão, embora seja bastante densa, devido à sintaxe exuberante do Bash.

Mas ainda assim é encantadora. O significado do programa e a intenção do
programador estão muito mais claros e são mais fáceis de entender.

Suspeitei, por bastante tempo, que a Filosofia Unix e a programação funcional
de alguma forma se complementam.

</section>
