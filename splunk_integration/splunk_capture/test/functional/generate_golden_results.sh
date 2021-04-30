#!/usr/bin/env bash

export PYTHON=`which python`;

export TEST_PATH="../../";

if [[ $? != 0 ]]; then
  export PYTHON=python;
fi

# color output 
export red=$'\e[1;31m';
export green=$'\e[1;32m';
export brown=$'\e[0;33m';
export yellow=$'\e[1;33m';
export blue=$'\e[0;34m';
export light_blue=$'\e[1;34m';
export magenta=$'\e[1;35m';
export cyan=$'\e[1;36m';
export white=$'\e[0m';

function test_help {  
  echo -e "${white}options:";
  echo -e " -load_list - Load the tests to generate golden results from a file.";
  echo -e " -no_write - Run the tests to generate golden results but don't write to file. Default writes to golden result files.\n";
  echo -e "arguments:";
  echo -e " [<test>] - Run single test. Optional.";
  echo -e " [<test> <test> <test>...] - Run multiple tests. Optional.\n";
  echo -e "examples:";
  echo -e " ./generate_golden_results.sh -load_list=test_list.txt";
  echo -e " ./generate_golden_results.sh -load_list=test_list.txt -no_write";
  echo -e " ./generate_golden_results.sh csv_input_test";
  echo -e " ./generate_golden_results.sh csv_input_test -no_write";
  echo -e " ./generate_golden_results.sh csv_input_test input_test kvstore_input_test";
  echo -e " ./generate_golden_results.sh csv_input_test input_test kvstore_input_test -no_write";
  exit;
}

function stop_golden_generation {
  echo -e "stop_golden_generation";
  local file=golden_files/locks/test.lock;
  rm $file;
  # unset the traps
  trap - SIGTSTP;
  trap - EXIT;
}

function start_golden_generation {
  # make sure that no matter what happens stop_tests gets run
  # SIGTSTP - no ctrl-z
  local file=golden_files/locks/test.lock;
  if [[ -f $file ]]; then
    echo -e "${red}The splunk_capture golden results generation is already being run in "`more $file`;
    exit;
  fi

  trap stop_golden_generation EXIT;
  trap "" SIGTSTP;
  echo -e "${brown}start_golden_generation `date`";
  ( cd ../..; echo -e `pwd` | xargs basename ) > $file;
}

if [[ ! -d golden_files/locks/ ]]; then
  mkdir -p golden_files/locks/;
fi

start_golden_generation;

# load a list file
function load_list {
  local file=$1;
 
  if [[ ! -f $file ]]; then
    echo -e "${red}-load_list invalid filename ${file}\n";
    exit;
  fi
  
  while read -r LINE; do
    export THE_TEST="$THE_TEST $LINE"    
  done < $file

  export TEST_LIST=1;
}

NO_WRITE=false;

CUR_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

for opt in "$@"; do
  if [[ $opt =~ "-help" || $opt =~ "-h" ]]; then  # run only the tests saved in this file
    test_help;
  elif [[ $opt =~ "-load_list" ]]; then  # run only the tests saved in this file
    load_list ${opt#*=};
  elif [[ $opt =~ "-no_write" ]]; then  # run only the tests saved in this file
    NO_WRITE=true;
  elif [[ ${opt:0:1} != '-' ]]; then 
    if [[ -n $THE_TEST ]]; then  # already created
      export THE_TEST="$THE_TEST $opt";
      export TEST_LIST=1;
    else 
      export THE_TEST=$opt;
    fi
  else
    echo -e "${red}\nUnknown option ${opt}\n"
    test_help;
  fi
done

if [[ -n $TEST_LIST ]]; then 
  declare -a THE_TEST=($THE_TEST);
  export THE_TEST=${THE_TEST[@]};
  echo -e "${magenta}----------------------------------------";
  echo -e "${cyan}Testing List: $THE_TEST";
  echo -e "${magenta}----------------------------------------";
fi

function check_test {
  if [[ -n $TEST_LIST ]]; then
    for i in ${THE_TEST[@]}; do 
      # =~ changes with the order of the args, i.e., both directions are necessary
      if [[ $i == $1  || $1 =~ $i || $i =~ $1 ]]; then 
        return 0;
      fi
    done
  else
    # =~ changes with the order of the args, i.e., both directions are necessary
    if [[ -z $THE_TEST || $THE_TEST == $1  || $1 =~ $THE_TEST || $THE_TEST =~ $1 ]]; then 
      return 0;
    fi
  fi
  return 1;
}

function run {
  check_test $2 || return 1;

  local options=$1;
  local test_file=$2;
  local base=golden_results/$2;
  local full_base=test/functional/golden_results/$2;
  local file=build/test/results/$test_file;
  local custom_command="";

  echo -e "\n${magenta}****************************************";
  echo -e "${white}generating $base";
  echo -e "${magenta}****************************************";
  
  local command=("./splunk_capture" "$options");

  if [[ -f "tests/${test_file}_custom" ]]; then      
    custom_command=`cat tests/${test_file}_custom`;
    echo -e "${light_blue}tests/${test_file}_custom found: $custom_command";      
    command=("./splunk_capture" "$options" "$custom_command");
  fi  

  echo -e "${white}${command[@]} > $full_base"; 

  ( cd $TEST_PATH; eval "${command[@]}" ) &> $base
  echo -e "${magenta}----------------------------------------";
  
  return 0;
}

function run_no_write { 
  local options=$1;
  local test_file=$2;
  local custom_command="";

  echo -e "\n${magenta}****************************************";
  echo -e "${white}running $test_file";
  echo -e "${magenta}****************************************";

  local command=("./splunk_capture" "$options");

  if [ -f "tests/${test_file}_custom" ]; then      
      custom_command=`cat tests/${test_file}_custom`;
      echo -e "${light_blue}tests/${test_file}_custom found: $custom_command";      
      command=("./splunk_capture" "$options" "$custom_command");
  fi

  echo -e "${white}${command[@]}"; 

  echo -e $blue;
  ( cd $TEST_PATH; eval "${command[@]}" )
  echo -e "${magenta}----------------------------------------";

  return 0;
}

function start {
  let time_start1=`date +"%s"`;

  export TEST_PATH="../..";

  # run test against golden results
  # there should be the same number of test files as there are golden results
  let count=0; 
  for i in $THE_TEST;
    do       
      let count=$count+1;
      local file_name=`echo -e $i | sed 's:.*/::'`;
      local test_options=`cat tests/$file_name`;

      if $NO_WRITE = true; then
        run_no_write "$test_options" $file_name; 
      else
        run "$test_options" $file_name; 
      fi      
  done

  let time_start2=`date +"%s"`;
  let time_start1=$time_start2-$time_start1;
  echo -e "\n${green}seconds $time_start1";

  date;
}

echo -e "\n${magenta}running golden results generation from root of splunk_capture repo";
if [[ $NO_WRITE = true ]]; then
  echo -e "${yellow}NO WRITE mode";
fi

start;

# reset default color scheme
echo -en "\e[0m";
